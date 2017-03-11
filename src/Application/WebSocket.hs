{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecursiveDo #-}

module Application.WebSocket where

import Network.WebSockets ( defaultConnectionOptions, acceptRequest, sendTextData
                          , receiveDataMessage, DataMessage (Text))
import Network.WebSockets.RPC (execWebSocketServerRPCT, WebSocketServerRPCT, rpcServer, RPCServer, RPCServerParams(..))
import Application.Types (Env (..), AppM, runAppM, WebsocketException (UnsupportedReceiveData))
import Network.Wai.Trans (MiddlewareT, websocketsOrT, runServerAppT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A
import qualified Data.Map as Map
import Control.Monad (void, forever)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (throwM)
import Control.Concurrent (threadDelay, myThreadId, killThread)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar, readTVarIO, newTVarIO)
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan, tryReadTChan)


socket :: MiddlewareT AppM
socket app req resp = do
  env <- ask
  let middleware = websocketsOrT (runAppM env) defaultConnectionOptions $ \pendingConn -> do
        let wsServer =
              let wsServer' :: RPCServer () () () () AppM
                  wsServer' RPCServerParams{reply,complete} eSubSup =
                    case eSubSup of
                      Left () -> do
                        countRef <- liftIO $ newTVarIO 0
                        let go = do
                              count <- readTVarIO countRef
                              atomically $ modifyTVar countRef (+1)
                              if count < 5
                                then runAppM env $ reply ()
                                else do
                                  runAppM env $ complete ()
                                  killThread =<< myThreadId
                              threadDelay 1000000
                        void $ liftIO $ async $ forever go
                      Right () ->
                        liftIO $ print ()
              in  rpcServer wsServer'

        execWebSocketServerRPCT (wsServer pendingConn)

  middleware app req resp
