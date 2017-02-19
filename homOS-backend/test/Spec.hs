{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Data.Text (Text(..))
import Test.Hspec.Wai
import qualified Data.HashMap.Strict as HM
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

shouldMatchSchema :: HasCallStack => WaiSession SResponse -> (SResponse -> Maybe String) -> WaiExpectation
shouldMatchSchema action matcher = do
  r <- action
  forM_ (matcher r) (liftIO . expectationFailure)

matchSchema :: (HM.HashMap Text Value -> Bool) -> SResponse -> Maybe String
matchSchema f (SResponse (Status status _) headers body) = do
  guard $ status /= 200
  let (Object body') = asJSONValue body
  guard $ f body'
  pure ""

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
