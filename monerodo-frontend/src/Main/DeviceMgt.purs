module Main.DeviceMgt where

import Prelude
import Main.DeviceMgt.Logs as Logs
import Data.SemanticUI.Menu as Menu
import Data.SemanticUI.Grid as Grid
import Data.Zipper as Z
import Pux (Update, noEffects)
import Pux.Html.Attributes (className)
import Pux.Html.Elements (Attribute, Html, div, h3, text)
import Pux.Html.Events as PE


data Action
  = Unit
  | LogsAction Logs.Action

data State
  = LogsPage Logs.State
  | AdminPage
  | FirewallPage
  | UpgradePage
  | PowerPage

init :: Menu.Menu State
init = Menu.init $ Z.Zipper
  { _depth : []
  , _current : Menu.MenuItem
    { _title : "Logs"
    , _content : LogsPage Logs.init
    }
  , _forward :
    [ Menu.MenuItem
        { _title : "Admin"
        , _content : AdminPage
        }
    , Menu.MenuItem
        { _title : "Firewall"
        , _content : FirewallPage
        }
    , Menu.MenuItem
        { _title : "Upgrade"
        , _content : UpgradePage
        }
    , Menu.MenuItem
        { _title : "Power"
        , _content : PowerPage
        }
    ]
  }


update :: forall eff. Update (Menu.Menu State) (Menu.Action Action) eff
update = Menu.update updateApp
  where
    updateApp (LogsAction a) (LogsPage x) = case Logs.update a x of
      {state,effects} -> {state: LogsPage state, effects: map (map LogsAction) effects}
    updateApp _ x = noEffects x

view :: Menu.Menu State -> Html (Menu.Action Action)
view ms =
  Grid.multiRowGrid Grid.def []
    [ Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn []
            [h3 [className "ui dividing header"] [text "Device Management"]]
        ]
    , Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn [] $
            Menu.view viewApp ms
        ]
    ]
  where
    viewApp :: State -> Html Action
    viewApp (LogsPage x) = map LogsAction $ Logs.view x
    viewApp AdminPage = div [] [text "Admin Management"]
    viewApp FirewallPage = div [] [text "Firewall Management"]
    viewApp UpgradePage = div [] [text "Upgrade Management"]
    viewApp PowerPage = div [] [text "Power Management"]
