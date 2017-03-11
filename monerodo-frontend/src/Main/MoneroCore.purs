module Main.MoneroCore where

import Prelude
import Data.SemanticUI.Grid as Grid
import Data.SemanticUI.Switch as Switch
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Either (Either (..))
import Data.Foreign (parseJSON, F, unsafeFromForeign, Foreign)
import Pux (Update, noEffects)
import Pux.Html (Html, text, div, h3, h4, input, label, button)
import Pux.Html.Events (onClick, onInput)
import Pux.Html.Attributes (className, type_, value)
import Control.Monad.Aff (later')
import Control.Monad.Except (runExcept)


data Action
  = ChangeStatus Switch.Action
  | ChangeBoot Switch.Action
  | ChangeDownload Number
  | ChangeUpload Number
  | ChangeManaged Switch.Action
  | Upgrade
  | FinishedUpgrade

type State =
  { status    :: Boolean
  , boot      :: Boolean
  , download  :: Number -- Kilobytes FIXME into own type
  , upload    :: Number
  , managed   :: Boolean
  , upgrading :: Boolean
  }


-- FIXME load from websocket
init :: State
init =
  { status : false
  , boot : false
  , download : 8.0 * kilobyte
  , upload   : 1.0 * kilobyte
  , managed : false
  , upgrading : false
  }

kilobyte :: Number
kilobyte = 1024.0

update :: forall eff. Update State Action eff
update (ChangeStatus a) state@{status} =
  noEffects $ state {status = Switch.update a status}
update (ChangeBoot a) state@{boot} =
  noEffects $ state {boot = Switch.update a boot}
update (ChangeDownload d) state@{download} =
  noEffects $ state {download = d}
update (ChangeUpload u) state@{upload} =
  noEffects $ state {upload = u}
update (ChangeManaged a) state@{managed} =
  noEffects $ state {managed = Switch.update a managed}
update Upgrade state@{upgrading} = -- FIXME actually upgrade
  { state : state {upgrading = true}
  , effects :
       [ later' 1000 (pure FinishedUpgrade)
       ]
  }
update _ state = noEffects state

view :: State -> Html Action
view {status,boot,download,upload,managed,upgrading} =
  Grid.multiRowGrid Grid.def []
    [ Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn []
            [h3 [className "ui dividing header"] [text "Monero Core Management"]]
        ]
    , Grid.homoRow Grid.TwoCols []
        [ Grid.unsizedColumn []
            [ map ChangeStatus $ Switch.view (if status then "Turn Off" else "Turn On") status
            ]
        , Grid.unsizedColumn []
            [ map ChangeBoot $ Switch.view "Start on Boot" boot
            ]
        ]
    , Grid.homoRow Grid.TwoCols []
        [ Grid.unsizedColumn []
            [ h4 [className "ui header"] [text "Download Speed"]
            , div [className "ui right labeled fluid input"]
                [ input [ type_ "number"
                        , onInput $ \{target} ->
                            ChangeDownload $ fromMaybe 0.0 $ parseNumber target.value
                        , value $ show download
                        ] []
                , div [className "ui label"] [text "kB/s"]
                ]
            ]
        , Grid.unsizedColumn []
            [ h4 [className "ui header"] [text "Upload Speed"]
            , div [className "ui left labeled fluid input"]
                [ input [ type_ "number"
                        , onInput $ \{target} ->
                            ChangeUpload $ fromMaybe 0.0 $ parseNumber target.value
                        , value $ show upload
                        ] []
                , div [className "ui label"] [text "kB/s"]
                ]
            ]
        ]
    , Grid.homoRow Grid.TwoCols []
        [ Grid.unsizedColumn []
            [ map ChangeManaged $
              Switch.view (if managed then "Make Unmanaged" else "Make Managed") managed
            ]
        , button [ onClick $ const Upgrade
                 , className "ui button"
                 ] [text "Upgrade"]
        ]
    ]


parseNumber :: String -> Maybe Number
parseNumber value = hush $ runExcept $ unsafeFromForeign <$> (parseJSON value :: F Foreign)
  where
    hush :: forall e a. Either e a -> Maybe a
    hush (Left _) = Nothing
    hush (Right x) = Just x
