{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module TestEllipticCurve (tests) where

import EllipticCurve (Point (..), add)
import Test.HUnit

tests :: Test
tests = TestList [TestCase testAdd0, TestCase testAdd1, TestCase testAdd2]

eq :: (Eq a, Show a) => a -> a -> Assertion
eq = assertEqual ""

testAdd0 :: Assertion
testAdd0 = do
  let a = Infinite (5 :: Int) 7
  let b = Finite 2 5 5 7
  let c = Finite 2 (-5) 5 7
  eq (a `add` b) b
  eq (b `add` a) b
  eq (b `add` c) a

testAdd1 :: Assertion
testAdd1 = do
  let a = Finite (3 :: Int) 7 5 7
  let b = Finite (-1) (-1) 5 7
  eq (a `add` b) (Finite 2 (-5) 5 7)

testAdd2 :: Assertion
testAdd2 = do
  let a = Finite (-1 :: Int) (-1) 5 7
  eq (a `add` a) (Finite 18 77 5 7)
