{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
  FlexibleContexts #-}

module Routes where

import Application.Types (AppM, AppLinks(AppHome))
import Application.WebSocket (socket)
import Pages.NotFound (notFoundContent)
import Routes.Assets (assetRoutes)
import Templates.Master (htmlLight, html)

import Network.HTTP.Types (status404)
import Network.Wai.Trans (MiddlewareT)
import Web.Routes.Nested
       (match, matchHere, matchAny, l_, o_, (</>), action, get, text, RouterT)

routes :: RouterT (MiddlewareT AppM) sec AppM ()
routes = do
  matchHere (action homeHandle)
  match (l_ "socket" </> o_) socket
  matchAny (action notFoundHandle)
  assetRoutes
  where
    homeHandle = get $ html (Just AppHome) ""
    notFoundHandle =
      get $ do
        htmlLight status404 notFoundContent
        text "404 :("
