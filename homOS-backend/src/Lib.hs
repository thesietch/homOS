{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Time
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text
import Data.Text (Text(..))
import Data.List (find)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe, maybeToList)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- for accessing 3rd party APIs
import qualified PredictionsByStop
import PredictionsByStop (ModeElt(..), RouteElt(..), DirectionElt(..), TripElt(..))
import qualified StopsByLocation
import StopsByLocation (StopElt(..))
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data Commute = Commute
  { lat :: Text
  , lon :: Text
  , name :: Text
  , leaving :: UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Commute)

type API = "commutes" :> Capture "person" Text :> Get '[JSON] [Commute]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server _ = liftIO getCommutes

getCommutes :: IO [Commute]
getCommutes = do
  maybePredictions <- fmap decode getPredictionsJSON
  maybeStops <- fmap decode getStopsJSON
  let maybeCommute = do
        predictions <- maybePredictions
        stops <- maybeStops
        topLevelToCommute stops predictions
  pure $ maybeToList maybeCommute

outboundPredictionsURL :: String
outboundPredictionsURL = "http://realtime.mbta.com/developer/api/v2/predictionsbystop?api_key=wX9NwuHnZU2ToO7GmGR9uw&stop=99991&format=json"

getPredictionsJSON :: IO B.ByteString
getPredictionsJSON = simpleHttp outboundPredictionsURL

stopsByLocationURL :: String
stopsByLocationURL = "http://realtime.mbta.com/developer/api/v2/stopsbylocation?api_key=wX9NwuHnZU2ToO7GmGR9uw&lat=42.3097361&lon=-71.1151431&format=json"

getStopsJSON :: IO B.ByteString
getStopsJSON = simpleHttp stopsByLocationURL

-- modes -> routes -> directions -> trips -> trip -> trip.pre_dt
topLevelToCommute :: StopsByLocation.TopLevel -> PredictionsByStop.TopLevel -> Maybe Commute
topLevelToCommute (StopsByLocation.TopLevel {..}) (PredictionsByStop.TopLevel {..}) = Commute <$> lat <*> lon <*> name <*> timestamp
  where
    stop = find (\(StopElt {..}) -> stopEltStopId == topLevelStopId ) topLevelStop
    name = pure topLevelStopName
    lat = fmap stopEltStopLat stop
    lon = fmap stopEltStopLon stop
    timestamp = do
      ModeElt {modeEltRoute} <- listToMaybe topLevelMode
      RouteElt {routeEltDirection} <- listToMaybe modeEltRoute
      DirectionElt {directionEltTrip} <- listToMaybe routeEltDirection
      TripElt {tripEltPreDt} <- listToMaybe directionEltTrip
      pure $ parseTimeOrError True defaultTimeLocale "%s" (Data.Text.unpack tripEltPreDt)
