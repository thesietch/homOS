{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.String.Here
import Data.Char (isSpace)
import Data.String (fromString)

fixturize heredoc = fromString $ filter (not . isSpace) heredoc

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /commutes/:person" $ do
    it "responds with 200" $ do
      get "/commutes/alex" `shouldRespondWith` 200
    it "responds with [Commute]" $ do
      let commutes = fixturize [hereFile|./test/fixtures/commutes.json|]
      get "/commutes/alex" `shouldRespondWith` commutes
