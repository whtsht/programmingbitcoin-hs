module ECDSA (Signature (..)) where

data Signature = Signature {r :: Integer, s :: Integer}
  deriving (Show, Eq)
