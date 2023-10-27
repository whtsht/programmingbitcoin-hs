module FiniteField
  ( Element (..),
    Field (..),
  )
where

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
  (|^) :: a -> Int -> a

instance Field Int where
  (|+) = (+)
  (|-) = (-)
  (|*) = (*)
  (|/) = div
  (|^) = (^)

data Element = Element
  { num :: Int,
    prime :: Int
  }
  deriving (Show, Eq)

modExp :: Int -> Int -> Int -> Int
modExp _ 0 _ = 1
modExp value exp_ mod_ = (value * modExp value (exp_ - 1) mod_) `mod` mod_

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
