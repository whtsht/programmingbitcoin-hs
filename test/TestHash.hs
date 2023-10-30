module TestHash (tests) where

import Data.String (IsString (fromString))
import Hash (hash256, hexStringToByteString)
import Test.HUnit

tests :: Test
tests = TestList [TestCase testHash0]

eq :: (Eq a, Show a) => a -> a -> Assertion
eq = assertEqual ""

testHash0 :: Assertion
testHash0 =
  eq (hash256 (fromString "Hello world")) (hexStringToByteString "0xf6dc724d119649460e47ce719139e521e082be8a9755c5bece181de046ee65fe")
