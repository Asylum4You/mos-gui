module Data.SemanticUI.Button where

import Prelude
import Data.Foldable (intercalate)
import Pux.Html as P
import Pux.Html.Attributes as PA
import Pux.Html.Events as PE


-- | FIXME embed in component state
type ButtonConfig =
  { toggle   :: Boolean
  , disabled :: Boolean
  , fluid    :: Boolean
  , active   :: Boolean
  }

def :: ButtonConfig
def =
  { toggle : false
  , disabled : false
  , fluid : false
  , active : false
  }

button :: forall a. ButtonConfig -> Array (P.Attribute a) -> Array (P.Html a) -> P.Html a
button config attrs children =
  P.button
    ([PA.className $ "ui button " <> classes] <> attrs) children
  where
    classes = intercalate " "
      [ if config.toggle   then "toggle"   else ""
      , if config.disabled then "disabled" else ""
      , if config.fluid    then "fluid"    else ""
      , if config.active   then "active"   else ""
      ]
