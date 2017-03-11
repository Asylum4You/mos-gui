module Application.Types where

import Prelude
import WebSocket (WEBSOCKET, Connection)
import Data.Map (Map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Signal.Channel (CHANNEL)



type AppEffs eff = ( ref :: REF
                   , ws :: WEBSOCKET
                   , console :: CONSOLE
                   , timer :: TIMER
                   | eff)
type AllEffs eff = ( ref :: REF
                   , ws :: WEBSOCKET
                   , err :: EXCEPTION
                   , channel :: CHANNEL
                   , console :: CONSOLE
                   , timer :: TIMER
                   | eff)
