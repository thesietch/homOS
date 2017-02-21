{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Debug.Trace (traceShowId)
import qualified Data.Vector as V
import Test.Hspec
import Data.Text (Text(..))
import Test.Hspec.Wai
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Network.HTTP.Types (Status(..))
import Network.Wai.Test (SResponse(..))
import Test.Hspec.Wai.JSON
import Control.Monad (forM_, guard)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.String.Here
import Data.String (fromString, IsString)
import qualified Data.ByteString.Lazy.Char8 as B

asJSONValue :: B.ByteString -> Value
asJSONValue = fromMaybe (object []) . decode

fixturize :: FromValue a => B.ByteString -> a
fixturize = fromValue . asJSONValue

shouldConformTo :: HasCallStack => WaiSession SResponse -> (SResponse -> Expectation) -> WaiExpectation
shouldConformTo action matcher = do
  r <- action
  liftIO $ matcher r

jsonSchema :: (Value -> Expectation) -> SResponse -> Expectation
jsonSchema isValid (SResponse (Status status _) headers body) = do
  guard $ status == 200
  let body' = asJSONValue body
  isValid body'

isAllCommutes :: Value -> Expectation
isAllCommutes (Array v) = names `shouldBe` requiredNames
  where
    requiredNames = HS.fromList ["South St @ Bardwell St", "775 Centre St"]
    names = HS.fromList . map (\(Object e) -> HM.lookupDefault "idk" "name" e) $ V.toList v

isValidCommute :: Value -> Expectation
isValidCommute (Array v) = do
  providedKeys `shouldBe` requiredKeys
  withoutLeavingResponse `shouldBe` withoutLeavingFixture
  where
    (Object hm) = V.head v
    providedKeys = HS.fromList $ HM.keys hm
    requiredKeys = HS.fromList $ HM.keys fixtureElem
    withoutLeavingFixture = HM.delete "leaving" fixtureElem
    withoutLeavingResponse = HM.delete "leaving" hm
    (Array fixture) = asJSONValue [hereFile|./test/fixtures/commutes.json|]
    (Object fixtureElem) = V.head fixture

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /commutes/:person" $ do
    it "responds with 200" $ do
      get "/commutes/alex" `shouldRespondWith` 200
    it "responds with [Commute]" $ do
      get "/commutes/alex" `shouldConformTo` jsonSchema isValidCommute
    it "responds with all valid commutes for Alex" $ do
      get "/commutes/alex" `shouldConformTo` jsonSchema isAllCommutes
