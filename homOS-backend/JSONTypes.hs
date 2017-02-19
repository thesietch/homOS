{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module JSONTypes where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data AlertHeadersElt = AlertHeadersElt { 
    alertHeadersEltAlertId :: Double,
    alertHeadersEltEffectName :: Text,
    alertHeadersEltHeaderText :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON AlertHeadersElt where
  parseJSON (Object v) = AlertHeadersElt <$> v .:   "alert_id" <*> v .:   "effect_name" <*> v .:   "header_text"
  parseJSON _          = mzero


instance ToJSON AlertHeadersElt where
  toJSON     (AlertHeadersElt {..}) = object ["alert_id" .= alertHeadersEltAlertId, "effect_name" .= alertHeadersEltEffectName, "header_text" .= alertHeadersEltHeaderText]


data Vehicle = Vehicle { 
    vehicleVehicleLon :: Text,
    vehicleVehicleSpeed :: Text,
    vehicleVehicleLat :: Text,
    vehicleVehicleId :: Text,
    vehicleVehicleBearing :: Text,
    vehicleVehicleLabel :: Text,
    vehicleVehicleTimestamp :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Vehicle where
  parseJSON (Object v) = Vehicle <$> v .:   "vehicle_lon" <*> v .:   "vehicle_speed" <*> v .:   "vehicle_lat" <*> v .:   "vehicle_id" <*> v .:   "vehicle_bearing" <*> v .:   "vehicle_label" <*> v .:   "vehicle_timestamp"
  parseJSON _          = mzero


instance ToJSON Vehicle where
  toJSON     (Vehicle {..}) = object ["vehicle_lon" .= vehicleVehicleLon, "vehicle_speed" .= vehicleVehicleSpeed, "vehicle_lat" .= vehicleVehicleLat, "vehicle_id" .= vehicleVehicleId, "vehicle_bearing" .= vehicleVehicleBearing, "vehicle_label" .= vehicleVehicleLabel, "vehicle_timestamp" .= vehicleVehicleTimestamp]


data TripElt = TripElt { 
    tripEltPreDt :: Text,
    tripEltPreAway :: Text,
    tripEltSchDepDt :: Text,
    tripEltTripName :: Text,
    tripEltTripId :: Text,
    tripEltVehicle :: (Maybe (Vehicle:|:[(Maybe Value)])),
    tripEltTripHeadsign :: Text,
    tripEltSchArrDt :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON TripElt where
  parseJSON (Object v) = TripElt <$> v .:   "pre_dt" <*> v .:   "pre_away" <*> v .:   "sch_dep_dt" <*> v .:   "trip_name" <*> v .:   "trip_id" <*> v .:?? "vehicle" <*> v .:   "trip_headsign" <*> v .:   "sch_arr_dt"
  parseJSON _          = mzero


instance ToJSON TripElt where
  toJSON     (TripElt {..}) = object ["pre_dt" .= tripEltPreDt, "pre_away" .= tripEltPreAway, "sch_dep_dt" .= tripEltSchDepDt, "trip_name" .= tripEltTripName, "trip_id" .= tripEltTripId, "vehicle" .= tripEltVehicle, "trip_headsign" .= tripEltTripHeadsign, "sch_arr_dt" .= tripEltSchArrDt]


data DirectionElt = DirectionElt { 
    directionEltTrip :: [TripElt],
    directionEltDirectionId :: Text,
    directionEltDirectionName :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON DirectionElt where
  parseJSON (Object v) = DirectionElt <$> v .:   "trip" <*> v .:   "direction_id" <*> v .:   "direction_name"
  parseJSON _          = mzero


instance ToJSON DirectionElt where
  toJSON     (DirectionElt {..}) = object ["trip" .= directionEltTrip, "direction_id" .= directionEltDirectionId, "direction_name" .= directionEltDirectionName]


data RouteElt = RouteElt { 
    routeEltDirection :: [DirectionElt],
    routeEltRouteId :: Text,
    routeEltRouteName :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON RouteElt where
  parseJSON (Object v) = RouteElt <$> v .:   "direction" <*> v .:   "route_id" <*> v .:   "route_name"
  parseJSON _          = mzero


instance ToJSON RouteElt where
  toJSON     (RouteElt {..}) = object ["direction" .= routeEltDirection, "route_id" .= routeEltRouteId, "route_name" .= routeEltRouteName]


data ModeElt = ModeElt { 
    modeEltRouteType :: Text,
    modeEltRoute :: [RouteElt],
    modeEltModeName :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON ModeElt where
  parseJSON (Object v) = ModeElt <$> v .:   "route_type" <*> v .:   "route" <*> v .:   "mode_name"
  parseJSON _          = mzero


instance ToJSON ModeElt where
  toJSON     (ModeElt {..}) = object ["route_type" .= modeEltRouteType, "route" .= modeEltRoute, "mode_name" .= modeEltModeName]


data StopElt = StopElt { 
    stopEltStopLon :: Text,
    stopEltDistance :: Text,
    stopEltStopLat :: Text,
    stopEltParentStationName :: Text,
    stopEltStopId :: Text,
    stopEltStopName :: Text,
    stopEltParentStation :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON StopElt where
  parseJSON (Object v) = StopElt <$> v .:   "stop_lon" <*> v .:   "distance" <*> v .:   "stop_lat" <*> v .:   "parent_station_name" <*> v .:   "stop_id" <*> v .:   "stop_name" <*> v .:   "parent_station"
  parseJSON _          = mzero


instance ToJSON StopElt where
  toJSON     (StopElt {..}) = object ["stop_lon" .= stopEltStopLon, "distance" .= stopEltDistance, "stop_lat" .= stopEltStopLat, "parent_station_name" .= stopEltParentStationName, "stop_id" .= stopEltStopId, "stop_name" .= stopEltStopName, "parent_station" .= stopEltParentStation]


data TopLevel = TopLevel { 
    topLevelAlertHeaders :: (Maybe ([AlertHeadersElt])),
    topLevelMode :: (Maybe ([ModeElt])),
    topLevelStopId :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelStop :: (Maybe ([StopElt])),
    topLevelStopName :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:?? "alert_headers" <*> v .:?? "mode" <*> v .:?? "stop_id" <*> v .:?? "stop" <*> v .:?? "stop_name"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["alert_headers" .= topLevelAlertHeaders, "mode" .= topLevelMode, "stop_id" .= topLevelStopId, "stop" .= topLevelStop, "stop_name" .= topLevelStopName]




parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


