{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module EllipticCurve (Point (..), add, scale) where

import Data.Bits (shiftR)
import FiniteField (Field (..))

data Point v
  = Infinite {a :: v, b :: v}
  | Finite {x :: v, y :: v, a :: v, b :: v}
  deriving (Show, Eq)

add :: (Field v, Eq v) => Point v -> Point v -> Point v
add p1 p2 =
  case (p1, p2) of
    (Infinite _ _, b) -> b
    (a, Infinite _ _) -> a
    (Finite x1 y1 a b, Finite x2 y2 _ _) ->
      case (x1 == x2, y1 == y2) of
        -- case 1: x1 == x2 and y1 /= y2
        (True, False) -> Infinite a b
        -- case 2: x1 /= x2
        (False, _) -> do
          let s = (y2 |- y1) |/ (x2 |- x1)
          let x3 = (s |^ 2) |- x1 |- x2
          let y3 = s |* (x1 |- x3) |- y1
          Finite x3 y3 a b
        -- case 3: P1 == P2
        (True, True) -> do
          let x12 = x1 |^ 2
          let s = ((x12 |+ x12 |+ x12) |+ a) |/ (y1 |+ y1)
          let x3 = (s |^ 2) |- (x1 |+ x1)
          let y3 = (s |* (x1 |- x3)) |- y1
          Finite x3 y3 a b

scale :: (Field v, Eq v) => Point v -> Integer -> Point v
scale a b = scale_ a a b
  where
    scale_ :: (Field v, Eq v) => Point v -> Point v -> Integer -> Point v
    scale_ a _ 0 = case a of
      Finite _ _ a b -> Infinite a b
      i -> i
    scale_ a r b =
      if b `mod` 2 == 1
        then r `add` scale_ a (r `add` r) (b `shiftR` 1)
        else scale_ a (r `add` r) (b `shiftR` 1)
