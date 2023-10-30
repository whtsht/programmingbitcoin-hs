{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FiniteField
  ( Element (..),
    Field (..),
    modExp,
  )
where

import Data.Bits (shiftR)

class Field a where
  -- addition
  (|+) :: a -> a -> a

  -- subtraction
  (|-) :: a -> a -> a

  -- multiplication
  (|*) :: a -> a -> a

  -- division
  (|/) :: a -> a -> a

  -- power
  (|^) :: a -> Integer -> a

instance Field Integer where
  (|+) = (+)
  (|-) = (-)
  (|*) = (*)
  (|/) = div
  (|^) = (^)

data Element = Element
  { num :: Integer,
    prime :: Integer
  }
  deriving (Show, Eq)

modExp :: Integer -> Integer -> Integer -> Integer
modExp value exp_ mod_ = modExp_ value value exp_ mod_
  where
    modExp_ _ _ 0 _ = 1
    modExp_ val r exp_ mod_ =
      if exp_ `mod` 2 == 1
        then (r * modExp_ val ((r * r) `mod` mod_) (exp_ `shiftR` 1) mod_) `mod` mod_
        else modExp_ val ((r * r) `mod` mod_) (exp_ `shiftR` 1) mod_ `mod` mod_

instance Field Element where
  a |+ b =
    if prime a /= prime b
      then error "Cannot add two numbers in different Fields"
      else Element {num = (num a + num b) `mod` prime a, prime = prime a}

  a |- b =
    if prime a /= prime b
      then error "Cannot subtract two numbers in different Fields"
      else Element {num = (num a - num b) `mod` prime a, prime = prime a}

  a |* b =
    if prime a /= prime b
      then error "Cannot multiply two numbers in different Fields"
      else Element {num = (num a * num b) `mod` prime a, prime = prime a}

  a |^ exp_ = Element {num = modExp (num a) (exp_ `mod` (prime a - 1)) (prime a), prime = prime a}

  a |/ b =
    if prime a /= prime b
      then error "Cannot divide two numbers in different Fields"
      else Element {num = (num a * modExp (num b) (prime a - 2) (prime a)) `mod` prime a, prime = prime a}
