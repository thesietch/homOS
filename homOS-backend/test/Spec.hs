{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /commutes/:person" $ do
    it "responds with 200" $ do
      get "/commutes/alex" `shouldRespondWith` 200
    it "responds with [CommuteStart]" $ do
      let commutes = "[{\"lat\":\"42.309949\",\"lon\":\"-71.115345\",\"name\":\"775CentreSt\",\"leaving\":\"2017-04-23T18:25:43.511Z\"}]"
      get "/commutes/alex" `shouldRespondWith` commutes
