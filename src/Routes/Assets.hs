{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Routes.Assets where

import Application.Assets
       (app, jquery, semanticJs, semanticCss, semanticIcons)
import Application.Types (MonadApp)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.Wai.Trans (MiddlewareT)
import Web.Routes.Nested
       (RouterT, matchGroup, match, bytestring, action, get, l_, o_,
        (</>), FileExt(..))

assetRoutes
  :: MonadApp m
  => RouterT (MiddlewareT m) sec m ()
assetRoutes =
  matchGroup (l_ "static" </> o_) $ do
    matchOn JavaScript "Main.min" $ LBS.fromStrict app
    matchOn JavaScript "jquery.min" $ LBS.fromStrict jquery
    matchGroup (l_ "semantic" </> o_) $ do
      match (l_ "semantic" </> o_) $
        action $
        get $ do
          bytestring Css $ LBS.fromStrict semanticCss
          bytestring JavaScript $ LBS.fromStrict semanticJs
      match
        (l_ "themes" </> l_ "default" </> l_ "assets" </> l_ "fonts" </>
         l_ "icons" </>
         o_) $
        action $
        get $
        forM_ semanticIcons $ \(f, b) ->
          let (_, e) = T.breakOnEnd "." $ T.pack f
          in bytestring (Other e) $ LBS.fromStrict b

matchOn
  :: MonadApp m
  => FileExt -> T.Text -> LBS.ByteString -> RouterT (MiddlewareT m) sec m ()
matchOn e f = match (l_ f </> o_) . action . get . bytestring e
