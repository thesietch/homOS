{-# LANGUAGE OverloadedStrings #-}
module Destination where
import Web.HttpApiData
import Data.Monoid ((<>))

data Person = Alex
instance FromHttpApiData Person where
  parseUrlPiece "alex" = Right Alex
  parseUrlPiece x = Left $ "not a person: " <> x

data Location = Work
              | Appt
instance FromHttpApiData Location where
  parseUrlPiece "work" = Right Work
  parseUrlPiece "appt" = Right Appt
  parseUrlPiece x = Left $ "not a location: " <> x

data Destination = Destination Person Location

newtype StopId = StopId String

toStopId :: Destination -> StopId
toStopId (Destination Alex Work) = StopId "99991"
toStopId (Destination Alex Appt) = StopId "1939"
