{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Api where

import Users
import Coins
import Trade

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Swagger
import Servant.Swagger
import Servant.Server (err400)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (Maybe(..))
import Data.Version (Version(..), showVersion)
import GHC.Generics
import Control.Monad.Freer hiding (run)
import qualified Control.Monad.Freer as Fr
import Control.Monad.IO.Class
import Control.Lens 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Paths_vce as Paths


type API = 
      -- | List all of the users in the system so far 
       "users" :> Get '[JSON] [User]
  -- | Signup endpoint to enroll a new user 
  :<|> "signup" :> ReqBody '[JSON] User :> Post '[JSON] User
  -- | List of all coins currently trading in our exchange along with current prices in BTC and USD
  :<|> "coins" :> Get '[JSON] [Coin]      
  :<|> "coins" :> Capture "CoinType" CoinType :> Get '[JSON] Coin
  -- | Trading endpoints to convert currencies in your portfolio  
  -- A trade currently requires you to send up details about the user. Ideally, this would just be the identifying information for the user making the trade, and would be behind a layer of authentication that determines if the current user is allowed to make trades on behalf of the specified user. 
  :<|> "buy" :> ReqBody '[JSON] Trade :> Post '[JSON] User
  :<|> "sell" :> ReqBody '[JSON] Trade :> Post '[JSON] User

  -- | Pass in a user's portfolio and check it's value in both BTC and USD
  :<|> "appraise" :> ReqBody '[JSON] Portfolio :> Post '[JSON] Appraisal
  -- | Docs
  :<|> "api-docs" :> Get '[JSON] SwaggerStream

-- | Documentation stuff
newtype SwaggerStream = SwaggerStream {unSwaggerStream :: BS.ByteString}
  deriving (Generic)
instance ToSchema SwaggerStream where
  declareNamedSchema _ = return $
    NamedSchema (Just "SwaggerStream") binarySchema
instance MimeRender JSON SwaggerStream where
  mimeRender _ (SwaggerStream bs) = BL.fromStrict bs

getSwagger :: HasSwagger api
  => Proxy api
  -> Text 
  -> Version
  -> Text
  -> SwaggerStream
getSwagger pxy t ver descr =
  SwaggerStream $ BL.toStrict $ encode $
    toSwagger pxy
    & info.title .~ t
    & info.version .~ T.pack (showVersion ver)
    & info.description .~ Just descr
-- ^ End Documentation stuff




startApp :: IO ()
startApp = putStrLn "Starting up ViCE (Virtual Currency Exchange)" >> run 8080 app

app :: Application
app = serve api server



api :: Proxy API
api = Proxy

server :: Server API
server = fetchUsers
    :<|> signupUser 
    :<|> fetchAllCoinData
    :<|> fetchCoinData
    :<|> buyCoins 
    :<|> sellCoins 
    :<|> appraise
    :<|> return (getSwagger
            api
            "ViCE"
            Paths.version
            "Server for exchanging virtual currencies"
          )

-- testApp :: Application 
-- testApp = serve api serverStatic          
-- serverStatic :: Server API
-- serverStatic = fetchUsersStatic
--     :<|> signupUserStatic
--     :<|> fetchCoinDataStatic
--     :<|> buyCoins 
--     :<|> sellCoins 
--     :<|> return (getSwagger
--             api
--             "ViCE"
--             Paths.version
--             "Server for exchanging virtual currencies"
--           )    

-- fetchUsersStatic = return $ Fr.run $ interpret interpUserConst fetch
-- signupUserStatic x = return $ Fr.run $ interpret interpUserConst (signup x)
-- fetchCoinDataStatic = return $ Fr.run $ interpret interpCoinConst priceAll


-- | TODO : Since each route uses a different interpretter, it would be good to combine the ones that should go together into a single stack that can be called. 

fetchUsers = liftIO $ Fr.runM $ interpUserFileSystem fetch
signupUser x = liftIO $ Fr.runM $ interpUserFileSystem (signup x)
fetchAllCoinData = liftIO $ Fr.runM $ interpCoinRest priceAll
fetchCoinData x = liftIO $ Fr.runM $ interpCoinRest (price x)

buyCoins trd = trade buy trd
sellCoins trd = trade sell trd 

trade fxn trd = do 
  -- | Get the price of the coin requested to trade 
  cn <- liftIO $ Fr.runM $ interpCoinRest (price (coin trd))
  -- | Get the user account information 
  usrs <- liftIO $ Fr.runM $ interpUserFileSystem fetch
  let usr = findMatching (user trd) usrs
  let trd' = trd {user = usr}
  -- | Apply the trade 
  case fxn trd' cn of 
    Just x -> do 
      -- | Save the users data
      liftIO $ Fr.runM $ interpUserFileSystem (update x) 
      return x 
    Nothing -> throwError err400

appraise ptf = do 
  cns <- liftIO $ Fr.runM $ interpCoinRest priceAll
  return $ evaluate ptf cns