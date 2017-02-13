{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Routes where

import Routes.Assets     (assetRoutes)
import Application.Types (MonadApp, AppLinks (AppHome))
import Templates.Master  (htmlLight, html)
import Pages.NotFound    (notFoundContent)

import Web.Routes.Nested  (matchHere, matchAny, action, get, text, RouterT)
import Network.Wai.Trans  (MiddlewareT)
import Network.HTTP.Types (status404)


routes :: ( MonadApp m
          ) => RouterT (MiddlewareT m) sec m ()
routes = do
  matchHere (action homeHandle)
  matchAny (action notFoundHandle)
  assetRoutes
  where
    homeHandle = get $ html (Just AppHome) "home"
    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("

