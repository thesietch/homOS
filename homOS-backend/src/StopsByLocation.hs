{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module StopsByLocation where

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
    topLevelStop :: [StopElt]
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "stop"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["stop" .= topLevelStop]
