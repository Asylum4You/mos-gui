{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
  FlexibleContexts #-}

module Templates.Master where

import Application.Types
       (MonadApp, envAuthority, AppLinks, AssetLinks(..), MainLinks (Main))

import qualified Data.Text as T
import Data.Url (AbsoluteUrlT(runAbsoluteUrlT))
import Lucid (HtmlT, renderBST)
import Network.HTTP.Types (Status, status200)
import qualified Network.Wai.Middleware.ContentType.Types as CT
import Path.Extended (toLocation)
import Web.Page.Lucid (WebPage(..), template)
import Web.Routes.Nested
       (FileExtListenerT, bytestring, mapStatus, mapHeaders)

import Control.Monad.Morph (hoist)
import Control.Monad.Reader (lift, ask)
import Control.Monad.State (modify)
import Data.Default (def)
import qualified Data.HashMap.Strict as HM
import Data.Markup (deploy, Remote(Remote), Inline(Inline))
import qualified Data.Markup as M
import Data.Monoid

-- | Render without @mainTemplate@
htmlLight
  :: (MonadApp m)
  => Status -> HtmlT (AbsoluteUrlT m) a -> FileExtListenerT m ()
htmlLight s content = do
  hostname <- envAuthority <$> lift ask
  bs <- lift $ runAbsoluteUrlT (renderBST content) hostname
  bytestring CT.Html bs
  modify . HM.map $
    mapStatus (const s) . mapHeaders ([("content-Type", "text/html")] ++)

-- | Shortcut for rendering with a template
html
  :: (MonadApp m)
  => Maybe AppLinks -> HtmlT (AbsoluteUrlT m) () -> FileExtListenerT m ()
html mLink content = htmlLight status200 $ mainTemplate mLink content

masterPage
  :: MonadApp m
  => WebPage (HtmlT m ()) T.Text
masterPage =
  let page
        :: MonadApp m
        => WebPage (HtmlT m ()) T.Text
      page = def
  in page
     { pageTitle = "Monerodo Console"
     , styles =
         do host <- envAuthority <$> lift ask
            hoist (`runAbsoluteUrlT` host) $ do
              semanticCss <- lift (toLocation SemanticCss)
              deploy M.Css Remote semanticCss
            inlineStyles
     , bodyScripts =
         do host <- envAuthority <$> lift ask
            hoist (`runAbsoluteUrlT` host) $ do
              jquery <- lift (toLocation JQuery)
              deploy M.JavaScript Remote jquery
              semanticJs <- lift (toLocation SemanticJs)
              deploy M.JavaScript Remote semanticJs
              main <- lift (toLocation Main)
              deploy M.JavaScript Remote main
     }
  where
    inlineStyles
      :: MonadApp m
      => HtmlT m ()
    inlineStyles = deploy M.Css Inline ("body {background: gray;}" :: T.Text)

masterTemplate
  :: (MonadApp m)
  => Maybe AppLinks
  -> WebPage (HtmlT (AbsoluteUrlT m) ()) T.Text
  -> HtmlT (AbsoluteUrlT m) ()
  -> HtmlT (AbsoluteUrlT m) ()
masterTemplate _ = template

mainTemplate
  :: (MonadApp m)
  => Maybe AppLinks -> HtmlT (AbsoluteUrlT m) () -> HtmlT (AbsoluteUrlT m) ()
mainTemplate state = masterTemplate state masterPage

appendTitle :: WebPage (HtmlT m ()) T.Text
            -> T.Text
            -> WebPage (HtmlT m ()) T.Text
appendTitle page x = page {pageTitle = x <> " « " <> pageTitle page}
