{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Api 
import Users
import Coins
import Trade

import Data.Maybe

import Test.Hspec
import Test.Hspec.Wai hiding (pending)
import Test.Hspec.Wai.JSON

import Test.QuickCheck
import qualified Control.Monad.Freer as Fr


main :: IO ()
main = do 
  -- hspec appSpec
  hspec tradeSpec

appSpec :: Spec
appSpec = do 
  with (return app) $ do
    describe "GET /users" $ do
      it "responds with 200" $ do
        get "/users" `shouldRespondWith` 200
      it "responds with [User]" $ do
        let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
        get "/users" `shouldRespondWith` users

    describe "POST /signup " $ do 
      let pstBody = "{\"userFirstName\": \"Isaac\",\"userLastName\": \"Newton2\",\"userId\": 1}"
      let pstResp = "{\"userFirstName\": \"Isaac\",\"userLastName\": \"Newton2\",\"userPortfolio\": {\"usd\": 10000,\"ltc\": 0,\"doge\": 0,\"xmr\": 0,\"btc\": 0},\"userId\": 1}"
      it "responds with 200" $ do 
        post pstBody "/signup" `shouldRespondWith` 200
      it "responds with new portfolio" $ do 
        post pstBody "/signUp" `shouldRespondWith` pstResp

tradeSpec :: Spec
tradeSpec = do 
  describe "Trading functions" $ do 
    it "Doesn't allow trades without sufficient funds" 
      pending
      -- Individual test case, useful for GHCi testing as well. Property version is below
    it "sell . buy == Just : Inidividual Test" $ do 
      let usr = User (Just 1) "Jake" "Keuhlen" (Just initialPortfolio)
      let trade = Trade 2 BTC usr 
      let cn = Coin BTC (3278.59, Dollar)

      let (Just usr') = buy trade cn
      let trade' = Trade 2 BTC usr'
      Just usr == sell trade' cn

    it "respects prop_validTradeToJust" $ do 
      quickCheck $ 
        forAll genTrade $ \trd -> 
        forAll genCoin $ \cn -> 
           prop_validTradeToJust trd cn

    it "respects prop_buySell" $ do        
      quickCheck $ 
        forAll genTrade $ \trd -> 
        forAll genCoin $ \cn -> 
           prop_buySell trd cn
           
-- Realistically, these generators are pretty weak and ought to be tied together to make the tests easier to pass. As it is, you will many times generate a trade that isn't for the coin type you generate, so it will be ignored as a trivial case. This is the source of "Gave Up!" appearing when you run the tests           
genCoin :: Gen Coin 
genCoin = elements $ Fr.run $ Fr.interpret interpCoinConst priceAll

genTrade :: Gen Trade 
genTrade = Trade 
              <$> (getPositive <$> arbitrary) 
              <*> elements [minBound..maxBound]
              <*> elements (Fr.run $ Fr.interpret interpUserConst fetch)

prop_validTradeToJust :: Trade -> Coin -> Property
prop_validTradeToJust trade@(Trade amt cnt usr) cn@(Coin cnt' (cst, currency)) =
  isJust (buy trade cn) ==>   
  let trade' = Trade amt cnt usr' 
      usr' = fromJust $ buy trade cn 
  in isJust (sell trade' cn) ==> 
      let usr'' = fromJust $ sell trade' cn 
      in  usr == usr''

prop_buySell :: Trade -> Coin -> Property 
prop_buySell trade@(Trade amt cnt usr) cn@(Coin cnt' (cst, currency)) = 
  isJust (buy trade cn) ==> 
  let trade' = Trade amt cnt usr' 
      res = sell trade' cn
      usr' = fromJust $ buy trade cn
  in isJust res 

