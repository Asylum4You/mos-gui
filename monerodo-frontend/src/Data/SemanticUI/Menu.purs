module Data.SemanticUI.Menu where

import Prelude
import Data.Array as A
import Data.Zipper (Zipper (Zipper))
import Data.Zipper as Z
import Data.Maybe (Maybe (..))
import Pux as P
import Pux.Html (Html)
import Pux.Html.Attributes as PA
import Pux.Html.Events as PE
import Pux.Html.Elements (div, text, a)
import Data.Zipper (lookupClosest)

data MenuItem a = MenuItem
  { _title   :: String
  , _content :: a
  }

data Menu s = Menu
  { _items :: Zipper (MenuItem s)
  }


data Action a
  = To Int
  | ContentAction Int a
  | Broadcast a

init :: forall s. Zipper (MenuItem s) -> Menu s
init xs = Menu
  { _items: xs
  }

update :: forall s a eff. P.Update s a eff -> P.Update (Menu s) (Action a) eff
update updateContent action (Menu {_items}) =
  case action of
    To idx ->
      { state : Menu
          { _items : Z.to idx _items
          }
      , effects : []
      }
    ContentAction idx actionContent ->
      case lookupClosest idx _items of
        MenuItem {_content, _title} ->
          let x' = updateContent actionContent _content
          in  { state : Menu
                  { _items : Z.replaceClosest idx (MenuItem {_title, _content: x'.state}) _items
                  }
              , effects : map (map (ContentAction idx)) x'.effects
              }
    Broadcast actionContent -> case _items of
      Z.Zipper {_depth, _current, _forward} ->
        let f (MenuItem {_title, _content}) = case updateContent actionContent _content of
              {state, effects} -> {state: MenuItem {_title, _content: state}, effects}
            enflail xs = case A.uncons xs of
              Nothing -> {state: [], effects: []}
              Just {head,tail} ->
                let xs' = enflail tail
                in  { state: A.cons head.state xs'.state
                    , effects: head.effects <> xs'.effects
                    }
            ds = enflail $ map f _depth
            c  = f _current
            fs = enflail $ map f _forward
        in  { state : Menu { _items: Z.Zipper {_depth: ds.state, _current: c.state, _forward: fs.state}}
            , effects : map (map Broadcast) $ ds.effects <> c.effects <> fs.effects
            }

view :: forall s a. (s -> Html a) -> Menu s -> Array (Html (Action a))
view viewContent (Menu {_items}) =
  [ div [PA.className "ui top attached tabular menu"] $
      viewMenu _items
  , div [PA.className "ui bottom attached segment"]
      [ case _items of
          Zipper {_current} ->
            case _current of
              MenuItem {_content} ->
                map (ContentAction $ Z.activeIndex _items) $ viewContent _content
      ]
  ]
  where
    viewMenu :: Zipper (MenuItem s) -> Array (Html (Action a))
    viewMenu (Zipper {_depth, _current, _forward}) =
         A.mapWithIndex viewMenuItemInactive _depth
      <> [viewMenuItemActive _current]
      <> A.mapWithIndex (\i -> viewMenuItemInactive $ i + A.length _depth + 1) _forward
      where
        viewMenuItemActive :: MenuItem s -> Html (Action a)
        viewMenuItemActive (MenuItem {_title}) =
          a [ PA.className "active item"
            ] [text _title]

        viewMenuItemInactive :: Int -> MenuItem s -> Html (Action a)
        viewMenuItemInactive idx (MenuItem {_title}) =
          a [ PA.className "item"
            , PE.onClick $ const $ To idx
            ] [text _title]
