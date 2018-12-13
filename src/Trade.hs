{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Trade where 

import Users  
import Coins  

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
import Data.Swagger
import Data.Map (Map)
import Data.Data
import Data.List(find)
import qualified Data.Map as M

import Control.Monad.Freer


data Trade = Trade 
  { 
    amount :: Double
  , coin :: CoinType
  , user :: User
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Trade)
instance ToSchema Trade

constrEq :: (Data a) => a -> a -> Bool
constrEq x y = toConstr x == toConstr y

-- | Given a buy request and an updated Coin description, apply the transaction to the user. Nothing means the trade failed and the user could not be updated. i.e. trying to buy more than you can afford
-- The pattern guard here means that you know the coin in the trade and the coin metadata you've been given are the same. Neat trick! 
buy :: Trade -> Coin -> Maybe User 
buy (Trade amt cnx usr) (Coin cny price) | constrEq cnx cny = 
  if hasFunds (snd price) (amt * (fst price)) usr 
    then Just $ 
      -- | Remove base currency 
      (userPortfolio . traverse . (currToLens $ snd price)) %~ (subtract (amt * (fst price))) $ 
      -- | add the new coin 
      (userPortfolio . traverse . (coinToLens cnx)) %~ (+amt) $ usr 
    else Nothing 
  where
  -- | utility function for buying, checks if a user has the requisite amount of Currency 
  hasFunds :: Currency -> Double -> User -> Bool 
  hasFunds curr x usr = fromMaybe False $ do 
    p <- _userPortfolio usr
    return $ if accessor curr p < x then False else True
  -- FIXME: accessor can go away and use lenses instead
  accessor Bitcoin = _btc 
  accessor Dollar = _usd 
buy _ _ = Nothing 

-- | Given a sell request and an updated Coin description, apply the transaction to the user. Nothing means the trade failed and the user could not be updated. i.e. trying to sell coins you don't have
sell :: Trade -> Coin -> Maybe User 
sell (Trade amt cnx usr) (Coin cny price) | constrEq cnx cny = 
  if hasCoins cnx amt usr 
    then Just $ 
      -- | Remove coin for sale
      (userPortfolio . traverse . (coinToLens cnx)) %~ (`subtract` amt) $ 
      -- | add the new currency
      (userPortfolio . traverse . (currToLens $ snd price)) %~ (+(amt*(fst price))) $ usr 
    else Nothing 
  where 
  -- | utility function for selling, checks if a user has the requisite amount of CoinTypes 
  hasCoins :: CoinType -> Double -> User -> Bool 
  hasCoins ctyp x usr = fromMaybe False $ do 
    p <- _userPortfolio usr
    return $ if accessor ctyp p < x then False else True 
  -- FIXME: accessor can go away and use lenses instead
  accessor BTC = _btc 
  accessor XMR = _xmr
  accessor DOGE = _doge
  accessor LTC = _ltc 

sell _ _ = Nothing 

currToLens Bitcoin = btc 
currToLens Dollar = usd

coinToLens BTC = btc 
coinToLens XMR = xmr 
coinToLens DOGE = doge 
coinToLens LTC = ltc



data Appraisal = Appraisal 
  { 
    dollars  :: Double
  , bitcoins :: Double
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Appraisal)
instance ToSchema Appraisal

evaluate :: Portfolio -> [Coin] -> Appraisal
evaluate ptf cns = 
  let xmrVal = coinVal XMR 
      dogeVal = coinVal DOGE
      ltcVal = coinVal LTC 
      btcVal = coinVal BTC
      btcs = (xmrVal * _xmr ptf) 
           + (dogeVal * _doge ptf) 
           + (ltcVal * _ltc ptf) 
           + _btc ptf
           + (_usd ptf / btcVal)
      usds = btcs * btcVal
  in Appraisal usds btcs 

  where 
    coinVal x = fromMaybe 0 $ fst . coinPrice <$> find ((== x) . coinName) cns 