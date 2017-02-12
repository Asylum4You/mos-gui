{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module Application.Types where

import Data.Url
import Data.Typeable
import Path.Extended
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Logger


-- * Infrastructure of the App

-- The environment accessible from our application
data Env = Env
  { envAuthority :: UrlAuthority
  } deriving (Show, Eq)


type AppM = LoggingT (ReaderT Env IO)

runAppM :: Env -> AppM a -> IO a
runAppM env xs = runReaderT (runStderrLoggingT xs) env

type MonadApp m =
  ( MonadReader Env m
  , MonadIO m
  , MonadThrow m
  , MonadCatch m
  , Typeable m
  )


-- * Links

data AssetLinks
  = JQuery
  | SemanticJs
  | SemanticCss

instance ToPath AssetLinks Abs File where
  toPath JQuery      = parseAbsFile "/static/jquery"
  toPath SemanticJs  = parseAbsFile "/static/semantic/semantic"
  toPath SemanticCss = parseAbsFile "/static/semantic/semantic"

instance ToLocation AssetLinks Abs File where
  toLocation JQuery      = (addFileExt "min.js" . fromPath) <$> toPath JQuery
  toLocation SemanticJs  = (addFileExt "js"     . fromPath) <$> toPath SemanticJs
  toLocation SemanticCss = (addFileExt "css"    . fromPath) <$> toPath SemanticCss


data AppLinks
  = AppHome

instance ToPath AppLinks Abs File where
  toPath AppHome = parseAbsFile "/index"

instance ToLocation AppLinks Abs File where
  toLocation AppHome = (addFileExt "min.js" . fromPath) <$> toPath AppHome

