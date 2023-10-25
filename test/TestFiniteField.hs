{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module TestFiniteField (tests) where

import FiniteField
import Test.HUnit

tests :: Test
tests = TestList [TestCase testAdd, TestCase testSub, TestCase testMul, TestCase testPow, TestCase testDiv]

eq :: (Eq a, Show a) => a -> a -> Assertion
eq = assertEqual ""

testAdd :: Assertion
testAdd = do
  let a = Element 2 31
  let b = Element 15 31
  eq (a |+ b) (Element 17 31)

  let a = Element 17 31
  let b = Element 21 31
  eq (a |+ b) (Element 7 31)

testSub :: Assertion
testSub = do
  let a = Element 29 31
  let b = Element 4 31
  eq (a |- b) (Element 25 31)

  let a = Element 15 31
  let b = Element 30 31
  eq (a |- b) (Element 16 31)

testMul :: Assertion
testMul = do
  let a = Element 24 31
  let b = Element 19 31
  eq (a |* b) (Element 22 31)

testPow :: Assertion
testPow = do
  let a = Element 17 31
  eq (a |^ 3) (Element 15 31)

  let a = Element 5 31
  let b = Element 18 31
  eq ((a |^ 5) |* b) (Element 16 31)

testDiv :: Assertion
testDiv = do
  let a = Element 3 31
  let b = Element 24 31
  eq (a |/ b) (Element 4 31)

  let a = Element 17 31
  eq (a |^ (-3)) (Element 29 31)

  let a = Element 4 31
  let b = Element 11 31
  eq ((a |^ (-4)) |* b) (Element 13 31)
