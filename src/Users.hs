{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Users where 

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Maybe (Maybe(..))
import Data.Swagger (ToSchema(..))
import GHC.Generics
import Data.Maybe
import qualified Data.List as L

import Control.Monad.Freer
import Control.Lens


data User = User
  { _userId        :: Maybe Int
  , _userFirstName :: Text
  , _userLastName  :: Text
  , _userPortfolio :: Maybe Portfolio 
  } deriving (Eq, Show, Generic)

instance ToJSON User where 
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = drop 1}
instance ToSchema User
instance FromJSON User where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = drop 1}

data Portfolio = Portfolio 
  {
    _usd  :: Double
  , _btc  :: Double 
  , _xmr  :: Double 
  , _doge :: Double 
  , _ltc  :: Double 
  } deriving (Eq, Show, Generic)
instance ToJSON Portfolio where 
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = drop 1}
instance ToSchema Portfolio
instance FromJSON Portfolio where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = drop 1}

initialPortfolio = Portfolio 10000.00 0.0 0.0 0.0 0.0

data UserX r where 
  Fetch :: UserX [User] 
  Signup :: User -> UserX User 
  Update :: User -> UserX User 

fetch :: Member UserX effs =>  Eff effs [User]
fetch = send Fetch

signup :: Member UserX effs => User -> Eff effs User
signup = send . Signup

update :: Member UserX effs => User -> Eff effs User
update = send . Update

interpUserConst :: UserX ~> Eff effs
interpUserConst = \case
  Fetch -> return defaultUsers
  Signup x -> return $ x {_userPortfolio = Just initialPortfolio}
  Update x -> return x

-- | This could be extended further to be built on a "FileSystem" effect, but I'm just going to put it in IO for now. 
interpUserFileSystem :: (LastMember IO effs, Member IO effs) => Eff (UserX ': effs) ~> Eff effs
interpUserFileSystem = interpretM (\case 
  Fetch -> fromMaybe [] <$> decodeFileStrict "users.json"
  Signup x -> do
    users <- fromMaybe [] <$> decodeFileStrict "users.json"
    let x' = x {_userPortfolio = Just initialPortfolio}
    encodeFile "users.json" (x':users)
    return x'
  Update x -> do 
    users <- fromMaybe [] <$> decodeFileStrict "users.json"
    let (xs,ys) = fmap (drop 1) $ span (== x) users
    encodeFile "users.json" $ xs ++ (x:ys)
    return x 
  ) 


defaultUsers :: [User]
defaultUsers = [ User (Just 1) "Isaac" "Newton" (Just initialPortfolio)
        , User (Just 2) "Albert" "Einstein" (Just initialPortfolio)
        ]

-- | If you don't have a user in the list, right now we just return the original user. This should return a Maybe User and then have error handling deal with the user not being in the list
findMatching :: User -> [User] -> User 
findMatching usr xs = fromMaybe usr $ L.find (\x -> _userId x == _userId usr) xs


makeLenses ''User
makeLenses ''Portfolio        