{-# LANGUAGE OverloadedStrings #-}

module Application.WebSocket where

import Data.WebSocketRPC (IncomingWSRPC (..))
import Network.WebSockets ( defaultConnectionOptions, acceptRequest, sendTextData
                          , receiveDataMessage, DataMessage (Text))
import Application.Types (AppM, runAppM, WebsocketException (UnsupportedReceiveData))
import Network.Wai.Trans (MiddlewareT, websocketsOrT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A
import Control.Monad (void, forever)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (throwM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)


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
          sendTextData conn $ T.decodeUtf8 $ LBS.toStrict $ A.encode ()
          threadDelay 1000000

        -- main loop
        forever $ do
          d <- liftIO $ receiveDataMessage conn
          incoming <- case d of
            Text b -> do
              case A.decode b of
                Just r -> pure r
                _      -> throwM $ UnsupportedReceiveData d
            _      -> throwM $ UnsupportedReceiveData d

          case incoming of
            WSSub _ -> pure ()
            WSSup _ -> pure ()

  middleware app req resp
