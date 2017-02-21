{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy     as B
import           Data.List                (find)
import           Data.Maybe               (listToMaybe, maybeToList)
import           Data.Text                (Text (..))
import qualified Data.Text
import           Data.Time
import           Destination
import           Network.HTTP.Conduit     (simpleHttp)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           PredictionsByStop        (DirectionElt (..), ModeElt (..),
                                           RouteElt (..), TripElt (..))
import qualified PredictionsByStop
import           Servant
import           StopsByLocation          (StopElt (..))
import qualified StopsByLocation

data Commute = Commute
  { lat     :: Text
  , lon     :: Text
  , name    :: Text
  , leaving :: UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Commute)

type API = "commutes" :> Capture "person" Person :> Get '[JSON] [Commute]
  :<|> "commutes" :> Capture "person" Person :> Capture "location" Location :> Get '[JSON] [Commute]

startApp :: IO ()
startApp = do
  putStrLn "running on 127.0.0.1:8080"
  withStdoutLogger $ \aplogger -> do
      let settings = setPort 8080 . setHost "*" $ setLogger aplogger defaultSettings
      runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = commutesByPerson
  :<|> commutesByPersonAndLocation

commutesByPerson person = liftIO $ do
  work <- getCommutes (Destination person Work)
  appt <- getCommutes (Destination person Appt)
  return $ work ++ appt
commutesByPersonAndLocation person location = liftIO $ getCommutes (Destination person location)

getCommutes :: Destination -> IO [Commute]
getCommutes destination = do
  maybePredictions <- fmap decode $ getPredictionsJSON destination
  maybeStops <- fmap decode getStopsJSON
  let maybeCommute = do
        predictions <- maybePredictions
        stops <- maybeStops
        topLevelToCommute stops predictions
  pure $ maybeToList maybeCommute

outboundPredictionsURL :: StopId -> String
outboundPredictionsURL (StopId stopId) = "http://realtime.mbta.com/developer/api/v2/predictionsbystop?api_key=wX9NwuHnZU2ToO7GmGR9uw&stop=" ++ stopId ++ "&format=json"

getPredictionsJSON :: Destination -> IO B.ByteString
getPredictionsJSON destination = simpleHttp (outboundPredictionsURL $ toStopId destination)

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
