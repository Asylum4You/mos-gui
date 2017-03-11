module Main.Mining.Pool where

import Prelude
import Data.SemanticUI.Grid as Grid
import Data.SemanticUI.Switch as Switch
import Pux (Update, noEffects)
import Pux.Html (Html, text, div, h3, h4, input, label)
import Pux.Html.Events (onChange)
import Pux.Html.Attributes (className, type_)



data ServiceAction
  = ChangeStatus Switch.Action
  | ChangeBoot Switch.Action

type ServiceState =
  { status :: Boolean
  , boot   :: Boolean
  }

initService :: ServiceState -- FIXME fetch from websocket
initService =
  { status : false
  , boot   : false
  }

updateService :: forall eff. Update ServiceState ServiceAction eff
updateService (ChangeStatus a) {status,boot} = noEffects
  {boot, status: Switch.update a status}
updateService (ChangeBoot a) {status,boot} = noEffects
  {boot: Switch.update a boot, status}

viewService :: ServiceState -> Html ServiceAction
viewService {status,boot} =
  Grid.homoRow Grid.TwoCols []
    [ Grid.unsizedColumn []
        [map (ChangeStatus) $ Switch.view (if status then "Turn Off" else "Turn On") status]
    , Grid.unsizedColumn []
        [map (ChangeBoot)   $ Switch.view (if boot then "Don't Start on Boot" else "Start on Boot") boot]
    ]


data Action
  = ChangeLocal ServiceAction
  | ChangeExternal ServiceAction

type State =
  { local     :: ServiceState
  , external  :: ServiceState
  }

init :: State
init =
  { local : initService
  , external : initService
  }

update :: forall eff. Update State Action eff
update (ChangeLocal a) {local,external} = case updateService a local of
  {state,effects} -> {state: {external,local: state}, effects: map (map ChangeLocal) effects}
update (ChangeExternal a) {local,external} = case updateService a external of
  {state,effects} -> {state: {external: state,local}, effects: map (map ChangeExternal) effects}

view :: State -> Html Action
view {local,external} =
  Grid.multiRowGrid Grid.def []
    [ Grid.homoRow Grid.OneCols []
        [Grid.unsizedColumn [] [h3 [className "ui dividing header"] [text "Pool Mining Settings"]]]
    , Grid.homoRow Grid.OneCols []
        [Grid.unsizedColumn [] [h4 [className "ui header"] [text "Local Pool"]]]
    , map ChangeLocal $ viewService local
    , Grid.homoRow Grid.OneCols []
        [Grid.unsizedColumn [] [h4 [className "ui header"] [text "External Pool"]]]
    , map ChangeExternal $ viewService external
    ]
