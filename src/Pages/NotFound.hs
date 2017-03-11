{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
  FlexibleContexts #-}

module Pages.NotFound where

import Application.Types

import qualified Data.Text as T
import Data.Url (AbsoluteUrlT, locUrl)
import Lucid (HtmlT, meta_, content_, div_, a_, h1_, p_, href_)
import Lucid.Base (makeAttribute)
import Path.Extended (toLocation)
import Web.Page.Lucid (WebPage, metaVars, template)

import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.Monoid ((<>))

notFoundContent
  :: (MonadApp m)
  => HtmlT (AbsoluteUrlT m) ()
notFoundContent = do
  home <- T.pack <$> lift (locUrl =<< toLocation AppHome)
  let page
        :: Monad m
        => WebPage (HtmlT m ()) T.Text
      page =
        def
        { metaVars =
            meta_
              [ makeAttribute "http-equiv" "refresh"
              , content_ $ "3;url=" <> home
              ]
        }
  template page $
    div_ [] $ do
      h1_ [] "Not Found!"
      p_ [] $ do
        "Your request was not found. Redirecting you to "
        a_ [href_ home] "the homepage"
        "..."
