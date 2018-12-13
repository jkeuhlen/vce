{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coins where 

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens
import Control.Lens
import Data.Text (Text)
import Text.Read(readMaybe)
import qualified Data.Text as T
import Data.Maybe (Maybe(..))
import GHC.Generics
import Data.Maybe
import Data.Scientific
import Data.Swagger
import Data.Data
import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.Freer
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.HttpApiData

data Coin = Coin 
  {
    coinName     :: CoinType 
  , coinPrice :: (Double, Currency)  
  } deriving (Eq, Show, Generic)
data Currency = Bitcoin | Dollar deriving (Eq, Show, Generic)
data CoinType = BTC | XMR | DOGE | LTC deriving (Ord, Eq, Show, Enum, Bounded, Generic, Data, Typeable)

instance FromHttpApiData CoinType where 
  parseUrlPiece "BTC" = Right BTC
  parseUrlPiece "XMR" = Right XMR
  parseUrlPiece "DOGE" = Right DOGE
  parseUrlPiece "LTC" = Right LTC
  parseUrlPiece x = Left $ "unknown url" <> x

$(deriveJSON defaultOptions ''CoinType)
$(deriveJSON defaultOptions ''Currency)
$(deriveJSON defaultOptions ''Coin)
instance ToSchema Coin
instance ToSchema CoinType
instance ToParamSchema CoinType
instance ToSchema Currency

coinToLongName :: CoinType -> String 
coinToLongName BTC = "bitcoin"
coinToLongName XMR = "monero"
coinToLongName DOGE = "dogecoin"
coinToLongName LTC = "litecoin"

coins :: Map CoinType Coin
coins = M.fromList [ (BTC, Coin BTC (3278.59, Dollar))
       , (XMR, Coin XMR (0.01311490, Bitcoin))
       , (DOGE, Coin DOGE (0.00000062, Bitcoin))
       , (LTC, Coin LTC (0.00707725, Bitcoin))
       ]

data CoinX r where 
  Price :: CoinType -> CoinX Coin 
  PriceAll :: CoinX [Coin]

price :: Member CoinX effs => CoinType -> Eff effs Coin
price = send . Price

priceAll :: Member CoinX effs => Eff effs [Coin]
priceAll = send PriceAll

interpCoinConst :: CoinX ~> Eff effs
interpCoinConst = \case
  -- | This fromJust is total as long as coins contains [minBound..maxBound :: CoinType]
  Price x -> return . fromJust $ M.lookup x coins
  PriceAll -> return $ M.elems coins  

-- | This could be extended further to be built on a "Rest" effect, but I'm just going to put it in IO for now. 
interpCoinRest :: (LastMember IO effs, Member IO effs) => Eff (CoinX ': effs) ~> Eff effs
interpCoinRest = interpretM (\case 
  Price x -> fxn x
  PriceAll -> mapM fxn [minBound..maxBound]
  )
  where
    -- Why do we use two separate pricing authorities here? ViCE was first implemented with only BTC support which was later extended to more coins using coingecko. Coindesk doesn't easily support anything other than BTC so I found coingecko. Since I was mildly worried about rate limiting against free resources during testing, I just used two separate entities for price details. 
    fxn :: CoinType -> _  
    -- | Powered by CoinDesk : https://www.coindesk.com/price/bitcoin
    fxn BTC = do 
      manager <- newManager tlsManagerSettings 
      req <- parseRequest "https://api.coindesk.com/v1/bpi/currentprice.json"
      resp <- httpLbs req manager
      let priceText = ((decode :: _ -> Maybe Value) $ responseBody resp) ^. key "bpi" . key "USD" . key "rate" . asText
      let price = parseDouble priceText 
      return $ Coin BTC (price, Dollar)
    -- | Powered by CoinGecko API
    fxn x = do 
      manager <- newManager tlsManagerSettings 
      req <- parseRequest $ "https://api.coingecko.com/api/v3/simple/price?ids=" ++ coinToLongName x ++ "&vs_currencies=btc"
      resp <- httpLbs req manager 
      let priceVal = ((decode :: _ -> Maybe Value) $ responseBody resp) ^. key (T.pack $ coinToLongName x) . key "btc"
      let price = valueToDouble priceVal 
      return $ Coin x (price, Bitcoin)

    valueToDouble :: Maybe Value -> Double 
    valueToDouble (Just (Number x)) = toRealFloat x 
    valueToDouble _ = (-1)
    parseDouble priceText = fromMaybe (-1.0 :: Double) $ do  
      txt <- priceText
      readMaybe $ T.unpack $ T.filter (/= ',') txt