{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Application.WebSocket where

import Data.WebSocketRPC ( IncomingWSRPC (..), OutgoingWSRPC (..), getCancel
                         , Supply (..), Subscribe (..), Reply (..), Complete (..)
                         )
import Network.WebSockets ( defaultConnectionOptions, acceptRequest, sendTextData
                          , receiveDataMessage, DataMessage (Text))
import Application.Types (Env (..), AppM, runAppM, WebsocketException (UnsupportedReceiveData))
import Network.Wai.Trans (MiddlewareT, websocketsOrT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A
import qualified Data.Map as Map
import Control.Monad (void, forever)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (throwM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar, readTVarIO, newTVarIO)
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan, tryReadTChan)


socket :: MiddlewareT AppM
socket app req resp = do
  env <- ask
  let middleware = websocketsOrT (runAppM env) defaultConnectionOptions $ \pendingConn -> do
        conn <- liftIO $ do
          c <- acceptRequest pendingConn
          sendTextData c ("Accepted Connection" :: T.Text)
          pure c

        -- heartbeat
        void $ liftIO $ async $ forever $ do
          sendTextData conn $ T.decodeUtf8 $ LBS.toStrict $ A.encode WSPing
          threadDelay 1000000

        -- main loop
        forever $ do
          d <- liftIO $ receiveDataMessage conn
          incoming <- case d of
            Text b ->
              case A.decode b of
                Just r -> pure r
                _      -> throwM $ UnsupportedReceiveData d
            _      -> throwM $ UnsupportedReceiveData d

          case incoming of
            WSSub Subscribe{_sub_ident} -> do
              -- match on method, to figure some prescribed cont
              contsVar <- envWSConts <$> ask
              chan     <- liftIO newTChanIO
              countRef <- liftIO $ newTVarIO 0
              thread   <- liftIO $ async $ forever $ do
                mx <- atomically $ tryReadTChan chan
                case mx of
                  Nothing -> pure ()
                  Just params -> do
                    print params
                    pure () -- TODO do something productive
                count <- readTVarIO countRef
                atomically $ modifyTVar countRef (+1)
                if count < 5
                  then sendTextData conn $ A.encode Reply
                         { _rep_ident = _sub_ident
                         , _rep_params = ()
                         }
                  else do atomically $ modifyTVar contsVar $ Map.delete _sub_ident
                          sendTextData conn $ A.encode Complete
                            { _com_ident = _sub_ident
                            , _com_params = ()
                            }
                          conts <- readTVarIO contsVar
                          case Map.lookup _sub_ident conts of
                            Nothing -> pure ()
                            Just (_,thread) -> cancel thread
                threadDelay 1000000

              liftIO $ atomically $ modifyTVar contsVar $ Map.insert _sub_ident
                ( chan
                , thread
                )
              pure ()
            WSSup Supply{_sup_ident,_sup_cancel,_sup_params} -> do -- pure ()
              contsVar <- envWSConts <$> ask
              conts <- liftIO $ readTVarIO contsVar
              case Map.lookup _sup_ident conts of
                Nothing -> pure () -- TODO log on failure?
                Just (chan,thread) ->
                  if getCancel _sup_cancel
                  then liftIO $ cancel thread
                  else liftIO $ atomically $ writeTChan chan _sup_params
            WSPong  -> pure ()

  middleware app req resp
