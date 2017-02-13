{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts,
  MultiParamTypeClasses, DeriveGeneric #-}

module Application.Types where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Url (UrlAuthority)
import Network.WebSockets (DataMessage)
import Path.Extended
       (ToPath(toPath), ToLocation(toLocation), parseAbsFile, Abs, File,
        addFileExt, fromPath)
import GHC.Generics (Generic)

-- * Infrastructure of the App
-- The environment accessible from our application
data Env = Env
  { envAuthority :: UrlAuthority
  } deriving (Show, Eq)

type AppM = LoggingT (ReaderT Env IO)

runAppM :: Env -> AppM a -> IO a
runAppM env xs = runReaderT (runStderrLoggingT xs) env

type MonadApp m = (MonadReader Env m, MonadIO m, MonadThrow m, MonadCatch m)

-- * Links

-- | Represents the web app itself
data MainLinks = Main

instance ToPath MainLinks Abs File where
  toPath Main = parseAbsFile "/static/Main"

instance ToLocation MainLinks Abs File where
  toLocation Main = (addFileExt "min.js" . fromPath) <$> toPath Main

-- | Represents the different dependencies needed by the web app
data AssetLinks
  = JQuery
  | SemanticJs
  | SemanticCss

instance ToPath AssetLinks Abs File where
  toPath JQuery = parseAbsFile "/static/jquery"
  toPath SemanticJs = parseAbsFile "/static/semantic/semantic"
  toPath SemanticCss = parseAbsFile "/static/semantic/semantic"

instance ToLocation AssetLinks Abs File where
  toLocation JQuery = (addFileExt "min.js" . fromPath) <$> toPath JQuery
  toLocation SemanticJs = (addFileExt "js" . fromPath) <$> toPath SemanticJs
  toLocation SemanticCss = (addFileExt "css" . fromPath) <$> toPath SemanticCss

-- | Represents discrete RESTful routes
data AppLinks
  = AppHome

instance ToPath AppLinks Abs File where
  toPath AppHome = parseAbsFile "/index"

instance ToLocation AppLinks Abs File where
  toLocation AppHome = (addFileExt "min.js" . fromPath) <$> toPath AppHome


-- * Exceptions

data WebsocketException
  = UnsupportedReceiveData DataMessage
  deriving (Show, Eq, Generic)
instance Exception WebsocketException
