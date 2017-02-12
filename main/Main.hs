{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module Main where

import Application
import Application.Types

import           Options.Applicative
import Network.Wai.Trans
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types
import Web.Routes.Nested

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
  } deriving Generic

instance Monoid AppOpts where
  mempty = AppOpts Nothing Nothing
  mappend
    AppOpts
      { port = p
      , host = h
      }
    AppOpts
      { port = p'
      , host = h'
      } =
    AppOpts
      (getLast $ Last p <> Last p')
      (getLast $ Last h <> Last h')

instance Default AppOpts where
  def = AppOpts (Just 3000) (Just "localhost")

appOpts :: Parser AppOpts
appOpts = AppOpts <$> portOpt <*> hostOpt
  where
    portOpt = optional . option auto $
          long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "port to listen on - default '3000'"
    hostOpt = optional . strOption $
          long "host"
       <> short 'h'
       <> metavar "HOST"
       <> help "hostname to deploy URLs over - default 'localhost'"

main :: IO ()
main = do
  -- CLI Opts
  let opts :: ParserInfo AppOpts
      opts = info (helper <*> appOpts)
        ( fullDesc
       <> progDesc "Serve application from PORT over HOST"
       <> header "monerodo-backend - a web server" )

  (commandOpts :: AppOpts) <- execParser opts

  let config :: AppOpts
      config = def <> commandOpts

  entry (fromJust $ port config) =<< appOptsToEnv config


-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> IO Env
appOptsToEnv (AppOpts (Just p) (Just h)) = do
  let a = UrlAuthority
            { urlScheme  = "http"
            , urlSlashes = True
            , urlAuth    = Nothing
            , urlHost    = h
            , urlPort    = p <$ guard (p /= 80)
            }
  pure $ Env a
appOptsToEnv _ = error "impossible state"


-- | Entry point, post options parsing
entry :: Int -> Env -> IO ()
entry p env =
  run p $
      gzip def
    . logStdoutDev
    . application
    $ failApp
  where
    application :: Middleware
    application = runMiddlewareT (runAppM env) $
        contentMiddleware
      . securityMiddleware

    failApp :: Application
    failApp _ respond =
      respond $ textOnly "404!" status404 []
