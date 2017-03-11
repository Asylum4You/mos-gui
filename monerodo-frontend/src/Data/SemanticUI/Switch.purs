module Data.SemanticUI.Switch where

import Prelude
import Pux (Update, noEffects)
import Pux.Html (Html, text, div, input, label)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className, type_, checked)


data Action = Clicked

update :: Action -> Boolean -> Boolean
update Clicked x = not x

view :: String -> Boolean -> Html Action
view desc x =
  div [className "ui slider checkbox"]
    [ input [ type_ "checkbox"
            , onClick $ const Clicked
            , checked x
            ] []
    , label [] [text desc]
    ]
