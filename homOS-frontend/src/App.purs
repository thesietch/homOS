module App where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Weather as W

data Query a = NotYet a

type State = { weather :: Maybe W.State }

initialState :: State
initialState = { weather: Nothing }

type ChildQuery = W.Query <\/> Const Void
type ChildSlot = W.Slot \/ Void

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
    render state = HH.div_
      [ HH.div_ [ HH.slot' CP.cp1 W.Slot W.ui unit absurd ]
      ]
    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (ajax :: AX.AJAX | eff))
    eval (NotYet next) = do
      -- a <- H.query' CP.cp1 W.Slot (H.request)
      pure next
