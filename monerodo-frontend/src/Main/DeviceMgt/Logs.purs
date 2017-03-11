module Main.DeviceMgt.Logs where

import Prelude
import Data.SemanticUI.Grid as Grid
import Data.SemanticUI.Button as Button
import Pux (Update, noEffects)
import Pux.Html.Attributes (className, style)
import Pux.Html.Elements (Attribute, Html, div, h4, h5, text, a)
import Pux.Html.Events (onClick)

import Data.Tuple (Tuple (..))
import Data.Generic (class Generic, gEq)


-- TODO: Batch Line-oriented websocket


data DocumentExtremity
  = DocBegin
  | DocEnd

derive instance genericDocumentExtremity :: Generic DocumentExtremity

instance eqDocumentExtremity :: Eq DocumentExtremity where
  eq = gEq


type DocumentLog =
  { cache :: String -- FIXME List String
  , attached :: DocumentExtremity
  }

initDocumentLog :: DocumentLog
initDocumentLog =
  { cache: """
Look, just because I don't be givin' no man a foot massage don't make it right for Marsellus to throw Antwone into a glass motherfuckin' house, fuckin' up the way the nigger talks. Motherfucker do that shit to me, he better paralyze my ass, 'cause I'll kill the motherfucker, know what I'm sayin'?

You think water moves fast? You should see ice. It moves like it has a mind. Like it knows it killed the world once and got a taste for murder. After the avalanche, it took us a week to climb out. Now, I don't know exactly when we turned on each other, but I know that seven of us survived the slide... and only five made it out. Now we took an oath, that I'm breaking now. We said we'd say it was the snow that killed the other two, but it wasn't. Nature is lethal but it doesn't hold a candle to man.

The path of the righteous man is beset on all sides by the iniquities of the selfish and the tyranny of evil men. Blessed is he who, in the name of charity and good will, shepherds the weak through the valley of darkness, for he is truly his brother's keeper and the finder of lost children. And I will strike down upon thee with great vengeance and furious anger those who would attempt to poison and destroy My brothers. And you will know My name is the Lord when I lay My vengeance upon thee.

You think water moves fast? You should see ice. It moves like it has a mind. Like it knows it killed the world once and got a taste for murder. After the avalanche, it took us a week to climb out. Now, I don't know exactly when we turned on each other, but I know that seven of us survived the slide... and only five made it out. Now we took an oath, that I'm breaking now. We said we'd say it was the snow that killed the other two, but it wasn't. Nature is lethal but it doesn't hold a candle to man.
"""
  , attached: DocEnd
  }

data DocumentAction
  = Head
  | Tail

updateDocumentLog :: forall eff. Update DocumentLog DocumentAction eff
updateDocumentLog Head {cache,attached} = noEffects
  { cache
  , attached: DocBegin
  }
updateDocumentLog Tail {cache,attached} = noEffects
  { cache
  , attached: DocEnd
  }

viewDocumentLog :: DocumentLog -> Array (Html DocumentAction)
viewDocumentLog {cache,attached} =
  [ Grid.homoRow Grid.OneCols []
      [ Grid.unsizedColumn []
          [ div [style [Tuple "background" "#ddd", Tuple "padding" "1em"]]
              [ div [style [ Tuple "height" "10em"
                           , Tuple "overflow-y" "scroll"
                           ]]
                  [text cache]
              ]
          ] -- TODO console div
      ]
  , Grid.homoRow Grid.OneCols []
      [ Grid.unsizedColumn []
          [ div [className "ui fluid buttons"]
              [ Button.button (Button.def {active = attached == DocBegin})
                  [onClick $ const Head] [text "First"]
              , Button.button (Button.def {active = attached == DocEnd})
                  [onClick $ const Tail] [text "Last"]
              ]
          ]
      ]
  ]

data ArchiveLevel
  = ArchiveOne
  | ArchiveTwo
  | ArchiveThree
  | ArchiveFour
  | ArchiveFive
  | ArchiveSix
  | ArchiveSeven

derive instance genericArchiveLevel :: Generic ArchiveLevel

instance eqArchiveLevel :: Eq ArchiveLevel where
  eq = gEq

type Log =
  { archive :: ArchiveLevel
  , log :: DocumentLog
  }

initLog :: Log
initLog =
  { archive: ArchiveOne
  , log: initDocumentLog
  }

data LogAction
  = ChangeArchiveLevel ArchiveLevel
  | DocumentAction DocumentAction

updateLog :: forall eff. Update Log LogAction eff
updateLog (ChangeArchiveLevel l) {log,archive} = noEffects {log, archive: l}
updateLog (DocumentAction a) {log,archive} = case updateDocumentLog a log of
  {state,effects} -> { state : {archive, log: state}
                     , effects : map (map DocumentAction) effects
                     }

viewLog :: Log -> Array (Html LogAction)
viewLog {archive, log} =
  [ Grid.homoRow Grid.OneCols []
      [ Grid.unsizedColumn []
          [ h5 [className "ui header"] [text "Archive Level"]
          , div [className "ui seven item menu"]
              [ item ArchiveOne "1"
              , item ArchiveTwo "2"
              , item ArchiveThree "3"
              , item ArchiveFour "4"
              , item ArchiveFive "5"
              , item ArchiveSix "6"
              , item ArchiveSeven "7"
              ]
          , div [className "ui divider"] []
          ]
      ]
  ] <> map (map DocumentAction) (viewDocumentLog log)
  where
    item x t = a [ className $ "item" <> if x == archive then " active" else ""
                 , onClick $ const $ ChangeArchiveLevel x
                 ] [text t]

type State =
  { nvidia :: Log
  , bitmonero :: Log
  }

init :: State
init =
  { nvidia: initLog
  , bitmonero: initLog
  }

data Action
  = Unit
  | NvidiaAction LogAction
  | BitmoneroAction LogAction

update :: forall eff. Update State Action eff
update Unit state = noEffects state
update (NvidiaAction a) {nvidia, bitmonero} = case updateLog a nvidia of
  {state, effects} -> {state: {bitmonero, nvidia: state}, effects: map (map NvidiaAction) effects}
update (BitmoneroAction a) {nvidia, bitmonero} = case updateLog a bitmonero of
  {state, effects} -> {state: {nvidia, bitmonero: state}, effects: map (map BitmoneroAction) effects}


-- TODO: Line numbers & click hyper-references
view :: State -> Html Action
view {nvidia,bitmonero} =
  Grid.multiRowGrid Grid.def [] $
    [ Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn []
            [ h4 [className "ui dividing header"] [text "Logs"]
            , h4 [className "ui header"] [text "nVidia"]
            ]
        ]
    ] <> map (map NvidiaAction) (viewLog nvidia)
      <>
    [ Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn []
            [ div [className "ui divider"] []
            , h4 [className "ui header"] [text "bitmonero"]
            ]
        ]
    ] <> map (map BitmoneroAction) (viewLog bitmonero)
