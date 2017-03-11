module Main where

import Prelude
import Application.Types (AllEffs, AppEffs)
import Main.DeviceMgt as DeviceMgt
import Main.MoneroCore as MoneroCore
import Main.Mining as Mining
import Data.SemanticUI.Menu as Menu
import Data.SemanticUI.Grid as Grid
import Data.Zipper as Z
import Data.Argonaut (jsonParser, decodeJson)
import Data.Either (Either (..))
import Data.Enum (succ)
import Data.Maybe (Maybe (..))
import Data.Map as Map
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Ref (REF)
import Signal.Channel as Chan
import WebSocket ( newWebSocket, runMessage, runMessageEvent, Connection (..), URL (..))
import WebSocket.RPC (execWebSocketClientRPCT, WebSocketClientRPCT, rpcClient, RPCClient, ClientAppT)

import Pux (renderToDOM, start, Update, noEffects)
import Pux.Html (Html, text, div, h2)
import Pux.Html.Attributes (className)


data Action
  = DeviceMgtAction (Menu.Action DeviceMgt.Action)
  | MoneroCoreAction MoneroCore.Action
  | MiningAction (Menu.Action Mining.Action)
  | Unit

data State
  = DeviceMgtPage (Menu.Menu DeviceMgt.State)
  | MoneroCorePage MoneroCore.State
  | MiningPage (Menu.Menu Mining.State)
  | MiniNodoPage

init :: Menu.Menu State
init = Menu.init $ Z.Zipper
  { _depth : []
  , _current : Menu.MenuItem
      { _title : "Device"
      , _content : DeviceMgtPage DeviceMgt.init
      }
  , _forward :
      [ Menu.MenuItem
          { _title : "Monero Core"
          , _content : MoneroCorePage MoneroCore.init
          }
      , Menu.MenuItem
          { _title : "Mining"
          , _content : MiningPage Mining.init
          }
      , Menu.MenuItem
          { _title : "MiniNodo"
          , _content : MiniNodoPage
          }
      ]
  }


update :: forall eff
        . Update (Menu.Menu State) (Menu.Action Action) (AppEffs eff)
update action state =
  case Menu.update updateApp action state of
        {state,effects} ->
          { state
          , effects:
              let logUpdateEffs = case action of
                    -- FIXME hardcoded page matching by Menu index? laaaaaame
                    Menu.To 1 ->
                      [ liftEff $ pure $ Menu.ContentAction 1 Unit
                      ]
                    _ -> []
              in effects <> logUpdateEffs
          }
  where
    updateApp (DeviceMgtAction a) (DeviceMgtPage x) =
      case DeviceMgt.update a x of
        {state,effects} ->
          { state   : DeviceMgtPage state
          , effects : map (map DeviceMgtAction) effects
          }
    updateApp (MoneroCoreAction a) (MoneroCorePage x) =
      case MoneroCore.update a x of
        {state,effects} ->
          { state   : MoneroCorePage state
          , effects : map (map MoneroCoreAction) effects
          }
    updateApp (MiningAction a) (MiningPage x) =
      case Mining.update a x of
        {state,effects} ->
          { state   : MiningPage state
          , effects : map (map MiningAction) effects
          }
    updateApp _ x = noEffects x

view :: Menu.Menu State -> Html (Menu.Action Action)
view ms =
  Grid.multiRowGrid {container: true} []
    [ Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn []
            [h2 [className "ui header"] [text "Monerodo Console"]]
        ]
    , Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn [] $
            Menu.view viewApp ms
        ]
    ]
  where
    viewApp :: State -> Html Action
    viewApp (DeviceMgtPage  x) = map DeviceMgtAction  $ DeviceMgt.view x
    viewApp (MoneroCorePage x) = map MoneroCoreAction $ MoneroCore.view x
    viewApp (MiningPage     x) = map MiningAction     $ Mining.view x
    viewApp MiniNodoPage = div [] [text "MiniNodo Management"]


main :: forall eff. Eff (AllEffs eff) Unit
main = do
  wsChan      <- Chan.channel Unit

  let wsClient :: ClientAppT (WebSocketClientRPCT (Array Unit) (Array Unit) (Eff (AllEffs eff))) Unit
      wsClient = rpcClient \dispatch -> do
        let wsClient' :: RPCClient (Array Unit) (Array Unit) (Array Unit) (Array Unit) (Eff (AllEffs eff))
            wsClient' =
              { subscription: ([] :: Array Unit)
              , onReply: \{supply, cancel} (x :: Array Unit) ->
                  pure unit
              , onComplete: \(x :: Array Unit) ->
                  pure unit
              }
        dispatch wsClient'

  conn <- newWebSocket (URL "ws://localhost:3000/socket") [] -- FIXME hostname?
  execWebSocketClientRPCT (wsClient conn)

  app <- start
    { initialState : init
    , update       : update
    , view         : view
    , inputs       : [map Menu.Broadcast $ Chan.subscribe wsChan] -- FIXME: why broadcast, when the callee might originate from a Menu page?
    }
  renderToDOM "#app" app.html
