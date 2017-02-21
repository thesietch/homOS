{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Destination where
import           Data.Aeson.TH
import           Data.Monoid     ((<>))
import           Web.HttpApiData

data Person = Alex | Other
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''Person)
instance FromHttpApiData Person where
  parseUrlPiece "alex" = Right Alex
  parseUrlPiece x = Left $ "not a person: " <> x

data Location = Work
              | Appt
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''Location)
instance FromHttpApiData Location where
  parseUrlPiece "work" = Right Work
  parseUrlPiece "appt" = Right Appt
  parseUrlPiece x = Left $ "not a location: " <> x

data Destination = Destination Person Location
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''Destination)

newtype StopId = StopId String

toStopId :: Destination -> StopId
toStopId (Destination Alex Work) = StopId "99991"
toStopId (Destination Alex Appt) = StopId "1939"
