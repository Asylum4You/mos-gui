{-# LANGUAGE
    FlexibleContexts
  , DeriveGeneric
  , OverloadedStrings
  #-}

module Application where

import Application.Types
import Routes

import Web.Routes.Nested
import Network.Wai.Trans

import Control.Monad.Catch

import GHC.Generics



data AuthRole = NeedsLogin

data AuthError = NeedsAuth
  deriving (Generic, Show)

instance Exception AuthError


authorize :: ( MonadThrow m
             ) => Request -> [AuthRole] -> m ()
authorize _ ss
  | null ss   = pure ()
  | otherwise = throwM NeedsAuth


securityMiddleware :: MonadApp m => MiddlewareT m
securityMiddleware app req resp = do
  extractAuth authorize req routes
  app req resp


contentMiddleware :: MonadApp m => MiddlewareT m
contentMiddleware =
  route routes
