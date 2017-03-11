module Main.Mining where

import Prelude
import Main.Mining.General as General
import Main.Mining.Nvidia as Nvidia
import Main.Mining.Cpu as Cpu
import Main.Mining.Pool as Pool
import Data.SemanticUI.Menu as Menu
import Data.SemanticUI.Grid as Grid
import Data.Zipper as Z
import Pux (Update, noEffects)
import Pux.Html (Html, text, div, h3)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className)


data Action
  = Unit
  | GeneralAction General.Action
  | NvidiaAction Nvidia.Action
  | CpuAction Cpu.Action
  | PoolAction Pool.Action

data State
  = GeneralPage General.State
  | NvidiaPage Nvidia.State
  | CpuPage Cpu.State
  | PoolPage Pool.State

init :: Menu.Menu State
init = Menu.init $ Z.Zipper
  { _depth: []
  , _current: Menu.MenuItem
      { _title: "General"
      , _content: GeneralPage General.init
      }
  , _forward:
      [ Menu.MenuItem
          { _title: "nVidia"
          , _content: NvidiaPage Nvidia.init
          }
      , Menu.MenuItem
          { _title: "CPU"
          , _content: CpuPage Cpu.init
          }
      , Menu.MenuItem
          { _title: "Pool"
          , _content: PoolPage Pool.init
          }
      ]
  }

update :: forall eff. Update (Menu.Menu State) (Menu.Action Action) eff
update = Menu.update updateApp
  where
    updateApp :: Update State Action eff
    updateApp (GeneralAction a) (GeneralPage x) = case General.update a x of
      {state,effects} -> {state: GeneralPage state, effects: map (map GeneralAction) effects}
    updateApp (NvidiaAction a) (NvidiaPage x) = case Nvidia.update a x of
      {state,effects} -> {state: NvidiaPage state, effects: map (map NvidiaAction) effects}
    updateApp (CpuAction a) (CpuPage x) = case Cpu.update a x of
      {state,effects} -> {state: CpuPage state, effects: map (map CpuAction) effects}
    updateApp (PoolAction a) (PoolPage x) = case Pool.update a x of
      {state,effects} -> {state: PoolPage state, effects: map (map PoolAction) effects}
    updateApp _ state = noEffects state


view :: Menu.Menu State -> Html (Menu.Action Action)
view ms =
  Grid.multiRowGrid Grid.def []
    [ Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn []
            [h3 [className "ui dividing header"] [text "Mining Management"]]
        ]
    , Grid.homoRow Grid.OneCols []
        [ Grid.unsizedColumn [] $
            Menu.view viewApp ms
        ]
    ]
  where
    viewApp :: State -> Html Action
    viewApp (GeneralPage x) = map GeneralAction $ General.view x
    viewApp (NvidiaPage x) = map NvidiaAction $ Nvidia.view x
    viewApp (CpuPage x) = map CpuAction $ Cpu.view x
    viewApp (PoolPage x) = map PoolAction $ Pool.view x
