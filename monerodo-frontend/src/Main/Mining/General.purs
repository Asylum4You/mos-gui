module Main.Mining.General where

import Prelude
import Pux (Update, noEffects)
import Pux.Html (Html, text, div, h3, input, label)
import Pux.Html.Events (onChange)
import Pux.Html.Attributes (className, type_)


data Action = ChangeAddress String

type State =
  { address :: String -- TODO: Base58
  } -- FIXME more?

init :: State -- FIXME fetch this from websocket
init =
  { address : ""
  }

update :: forall eff. Update State Action eff
update (ChangeAddress a) {address} = noEffects {address: a}

view :: State -> Html Action
view {address} =
  div []
    [ div [className "ui fluid right labeled input"]
        [ input [ type_ "text"
                , onChange $ \{target} -> ChangeAddress target.value
                ] []
        , div [className "ui label"] [text "Mining Address"]
        ]
    ]
