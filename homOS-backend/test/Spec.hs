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

shouldConformTo :: HasCallStack => WaiSession SResponse -> (SResponse -> Maybe String) -> WaiExpectation
shouldConformTo action matcher = do
  r <- action
  forM_ (matcher r) (liftIO . expectationFailure)

jsonSchema :: (Value -> Bool) -> SResponse -> Maybe String
jsonSchema isValid (SResponse (Status status _) headers body) = do
  guard $ status == 200
  let body' = asJSONValue body
  guard . not $ isValid body'
  pure "Schema does not match"

isValidCommute :: Value -> Bool
isValidCommute (Array v) = providedKeys == requiredKeys
  && withoutLeavingFixture == withoutLeavingResponse
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
