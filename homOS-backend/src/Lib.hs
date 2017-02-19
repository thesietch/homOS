{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Commute = Commute
  { lat :: String
  , lon :: String
  , name :: String
  , leaving :: String
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
server _ = return commutes

commutes :: [Commute]
commutes = [ Commute "42.309949" "-71.115345" "775CentreSt" "2017-04-23T18:25:43.511Z" ]
