{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module Main where

import Application
import Application.Types

import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import Network.Wai.Trans
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types
import Web.Routes.Nested

import System.Directory
import GHC.Generics

import Data.Url
import Data.Maybe
import Data.Default
import Data.Monoid
import Control.Monad.Reader



-- | Application-wide options
data AppOpts = AppOpts
  { port   :: Maybe Int
  , host   :: Maybe String
  , cwd    :: Maybe FilePath
  , static :: Maybe FilePath
  } deriving Generic

instance Monoid AppOpts where
  mempty = AppOpts Nothing Nothing Nothing Nothing
  (AppOpts p h c s) `mappend` (AppOpts p' h' c' s') =
    AppOpts
      (getLast $ Last p <> Last p')
      (getLast $ Last h <> Last h')
      (getLast $ Last c <> Last c')
      (getLast $ Last s <> Last s')

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

instance Default AppOpts where
  def = AppOpts (Just 3000) (Just "http://localhost") (Just ".") (Just "./static")

appOpts :: Parser AppOpts
appOpts = AppOpts <$> portOpt <*> hostOpt <*> cwdOpt <*> staticOpt
  where
    portOpt = optional . option auto $
          long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "port to listen on"
    hostOpt = optional . strOption $
          long "host"
       <> short 'h'
       <> metavar "HOST"
       <> help "host to deploy URLs over"
    cwdOpt = optional . strOption $
          long "cwd"
       <> short 'c'
       <> metavar "CWD"
       <> help "directory to search for files"
    staticOpt = optional . strOption $
          long "static"
       <> short 's'
       <> metavar "STATIC"
       <> help "directory to serve static files"
      

-- | Command-line options
data App = App
  { options    :: AppOpts
  , configPath :: Maybe String
  }

app :: Parser App
app = App
  <$> appOpts
  <*> optional ( strOption
        ( long "config"
       <> short 'c'
       <> metavar "CONFIG"
       <> help "path to config file" ))

main :: IO ()
main = do
  -- CLI Opts
  let opts :: ParserInfo App
      opts = info (helper <*> app)
        ( fullDesc
       <> progDesc "Serve application from PORT over HOST"
       <> header "monerodo-backend - a web server" )

  (commandOpts :: App) <- execParser opts

  -- Yaml Opts
  let yamlConfigPath = fromMaybe
        "config/config.yaml" $
        configPath commandOpts

  -- Yaml bug
  yamlConfigExists   <- doesFileExist yamlConfigPath
  yamlConfigContents <-
    if yamlConfigExists
    then readFile yamlConfigPath
    else pure ""

  mYamlConfig <-
    if yamlConfigExists && yamlConfigContents /= ""
    then Y.decodeFile yamlConfigPath
    else pure Nothing

  let yamlConfig :: AppOpts
      yamlConfig = fromMaybe def mYamlConfig

      config :: AppOpts
      config = def <> yamlConfig <> options commandOpts

  entry (fromJust $ port config) =<< appOptsToEnv config


-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> IO Env
appOptsToEnv (AppOpts (Just p) (Just h) (Just c) (Just s)) = do
  let a = UrlAuthority
            { urlScheme  = "http"
            , urlSlashes = True
            , urlAuth    = Nothing
            , urlHost    = h
            , urlPort    = p <$ guard (p /= 80)
            }
  pure $ Env a c s
appOptsToEnv (AppOpts _ _ _ _) = error "impossible state"


-- | Entry point, post options parsing
entry :: Int -> Env -> IO ()
entry p env =
  run p $
      staticPolicy (noDots >-> addBase "static")
    . gzip def
    . logStdoutDev
    . application
    $ failApp
  where
    application = runMiddlewareT (runAppM env) $
        contentMiddleware
      . securityMiddleware
      . staticMiddleware
    failApp _ respond =
      respond $ textOnly "404!" status404 []
