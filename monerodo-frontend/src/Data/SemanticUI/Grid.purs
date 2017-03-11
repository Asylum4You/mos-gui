module Data.SemanticUI.Grid where

import Prelude
import Data.Foldable (intercalate)
import Pux.Html.Attributes as PA
import Pux.Html.Elements (Attribute, Html, div)

import Data.Generic (class Generic, gEq)


-- * Grid

type GridConfig =
  { container :: Boolean
  }

def :: GridConfig
def = { container : false}

singleRowGrid :: forall a. GridConfig -> ColCount -> Array (Attribute a) -> Array (Html a) -> Html a
singleRowGrid {container} size attrs columns =
  div (attrs <> [PA.className $ "ui " <> show size <> " column grid " <> classes]) columns
  where
    classes = intercalate " "
      [ if container then "container" else ""
      ]

multiRowGrid :: forall a. GridConfig -> Array (Attribute a) -> Array (Html a) -> Html a
multiRowGrid {container} attrs children =
  div (attrs <> [PA.className $ "ui grid " <> classes]) children
  where
    classes = intercalate " "
      [ if container then "container" else ""
      ]

-- * Rows

homoRow :: forall a. ColCount -> Array (Attribute a) -> Array (Html a) -> Html a
homoRow size attrs columns =
  div (attrs <> [PA.className $ show size <> " column row"]) columns

heteroRow :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
heteroRow attrs columns =
  div (attrs <> [PA.className "row"]) columns

-- * Columns

unsizedColumn :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
unsizedColumn attrs children =
  div (attrs <> [PA.className "column"]) children

type SizedColumn a =
  { _size :: ColCount
  }

sizedColumn :: forall a. ColCount -> Array (Attribute a) -> Array (Html a) -> Html a
sizedColumn size attrs children =
  div (attrs <> [PA.className $ show size <> " wide column"]) children

data ColCount
  = OneCols
  | TwoCols
  | ThreeCols
  | FourCols
  | FiveCols
  | SixCols
  | SevenCols
  | EightCols
  | NineCols
  | TenCols
  | ElevenCols
  | TwelveCols

derive instance genericColCount :: Generic ColCount

instance eqColCount :: Eq ColCount where
  eq = gEq

instance showColCount :: Show ColCount where
  show OneCols = "one"
  show TwoCols = "two"
  show ThreeCols = "three"
  show FourCols = "four"
  show FiveCols = "five"
  show SixCols = "six"
  show SevenCols = "seven"
  show EightCols = "eight"
  show NineCols = "nine"
  show TenCols = "ten"
  show ElevenCols = "eleven"
  show TwelveCols = "twelve"
