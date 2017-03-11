module Data.Zipper where

import Prelude
import Data.Maybe (Maybe (Nothing, Just), fromMaybe)
import Data.Array ((!!), take, drop, unsnoc, uncons, cons, snoc, length)
import Data.Generic (class Generic, gShow, gEq)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple.Nested (T3)
import Data.Tuple (Tuple (Tuple))



-- | Trivial un-reversed Array-oriented zipper
data Zipper a = Zipper
  { _depth   :: Array a -- ^ sorted left-to-right
  , _current :: a
  , _forward :: Array a -- ^ sorted left-to-right
  }

derive instance genericZipper :: Generic a => Generic (Zipper a)

instance showZipper :: (Show a, Generic a) => Show (Zipper a) where
  show = gShow

instance eqZipper :: (Generic a, Eq a) => Eq (Zipper a) where
  eq = gEq

instance functorZipper :: Functor Zipper where
  map f (Zipper z) = Zipper
    { _depth  : map f z._depth
    , _current:     f z._current
    , _forward: map f z._forward
    }

-- | Leaves the left _current as the new _current
instance semigroupZipper :: Semigroup (Zipper a) where
  append (Zipper xs) (Zipper ys) = Zipper
    { _depth  : xs._depth
    , _current: xs._current
    , _forward: xs._forward <> ys._depth <> [ys._current] <> ys._forward
    }

instance monoidZipper :: Monoid a => Monoid (Zipper a) where
  mempty = Zipper
    { _depth  : []
    , _current: mempty
    , _forward: []
    }


-- | Instantiates a zipper by disregarding any initial depth.
fromArray :: forall a. Array a -> Maybe (Zipper a)
fromArray xs = do
  xs' <- uncons xs
  pure $ Zipper
    { _depth: []
    , _current: xs'.head
    , _forward: xs'.tail
    }

fromArray' :: forall a. a -> Array a -> Zipper a
fromArray' x xs = Zipper
  { _depth: []
  , _current: x
  , _forward: xs
  }

toArray :: forall a. Zipper a -> Array a
toArray (Zipper {_depth, _current, _forward}) = _depth <> [_current] <> _forward

-- | First element is fst
toArray' :: forall a. Zipper a -> Tuple a (Array a)
toArray' (Zipper {_depth, _current, _forward}) =
  case uncons _depth of
    Nothing          -> Tuple _current _forward
    Just {head,tail} -> Tuple head (tail <> [_current] <> _forward)

-- | Shifts backward, if possible
back :: forall a. Zipper a -> Zipper a
back x@(Zipper z) = case unsnoc z._depth of
  Nothing -> x
  Just {init,last} -> Zipper
    { _depth: init
    , _current: last
    , _forward: cons z._current z._forward
    }

forward :: forall a. Zipper a -> Zipper a
forward x@(Zipper z) = case uncons z._forward of
  Nothing -> x
  Just {head,tail} -> Zipper
    { _depth: snoc z._depth z._current
    , _current: head
    , _forward: tail
    }


activeIndex :: forall a. Zipper a -> Int
activeIndex (Zipper {_depth}) = length _depth


lookupClosest :: forall a. Int -> Zipper a -> a
lookupClosest i (Zipper {_depth, _current, _forward}) =
  let dLen = length _depth
  in if i < dLen
  then case uncons _depth of
         Nothing -> _current
         Just {head} -> fromMaybe head $ _depth !! i
  else if i > dLen
  then case unsnoc _forward of
         Nothing -> _current
         Just {last} -> fromMaybe last $ _forward !! (i-dLen-1)
  else _current

replaceClosest :: forall a. Int -> a -> Zipper a -> Zipper a
replaceClosest i x (Zipper {_depth, _current, _forward}) =
  let dLen = length _depth
  in if i < dLen
  then case splittingIndex i _depth of
         (Tuple _ (Tuple Nothing r)) -> -- i guaranteed to be < length, therefore l == [] when Nothing
           case uncons r of -- replace head
             Nothing     -> Zipper {_depth: [], _current: x, _forward}
             Just {tail} -> Zipper {_depth: [x] <> tail, _current, _forward}
         (Tuple l (Tuple (Just _) r)) ->
           Zipper {_depth: l <> [x] <> r, _current, _forward}
  else if i > dLen
  then case splittingIndex (i-dLen-1) _forward of -- (i-dLen-1) guaranteed to be >= 0, therefore r == [] when Nothing
         (Tuple l (Tuple (Just _) r)) ->
           Zipper {_depth, _current, _forward: l <> [x] <> r}
         (Tuple l (Tuple Nothing _)) ->
           case unsnoc l of -- replace last
             Nothing     -> Zipper {_depth, _current: x, _forward: []}
             Just {init} -> Zipper {_depth, _current, _forward: init <> [x]}
  else Zipper {_depth, _current: x, _forward}


-- | Will attempt to reach index, as closely as possible
to :: forall a. Int -> Zipper a -> Zipper a
to i z'@(Zipper z) =
  let dLen = length z._depth
  in if i == dLen
  then z' -- already at index
  else if i < dLen
  then case splittingIndex i z._depth of
         Tuple d' (Tuple mx f') -> case mx of
            Nothing ->
              case uncons z._depth of
                Nothing -> z'
                Just {head,tail} -> Zipper
                  { _depth: []
                  , _current: head
                  , _forward: tail <> cons z._current z._forward
                  }
            Just x  -> Zipper
              { _depth: d'
              , _current: x
              , _forward: f' <> cons z._current z._forward
              }
  else case splittingIndex (i-dLen-1) z._forward of
         Tuple d' (Tuple mx f') -> case mx of
            Nothing ->
              case unsnoc z._forward of
                Nothing -> z'
                Just {init,last} -> Zipper
                  { _depth: snoc z._depth z._current <> init
                  , _current: last
                  , _forward: []
                  }
            Just x  -> Zipper
              { _depth: snoc z._depth z._current <> d'
              , _current: x
              , _forward: f'
              }


splittingIndex :: forall a. Int -> Array a -> T3 (Array a) (Maybe a) (Array a)
splittingIndex i xs =
  if i >= length xs
  then Tuple xs (Tuple Nothing [])
  else if i < 0
  then Tuple [] (Tuple Nothing xs)
  else let mx = xs !! i
           l = take i xs
           r = drop (i+1) xs
       in  Tuple l (Tuple mx r)
