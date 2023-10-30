{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Secp256k1 (Secp256k1Elem, secp256k1Elem, secp256k1Point, verify, g, n, PrivateKey (..), privateKey, sign) where

import ECDSA (Signature (..))
import EllipticCurve (Point (Finite, Infinite), add, scale)
import FiniteField (Element (Element), modExp)

a :: Integer
a = 0

b :: Integer
b = 7

p :: Integer
p = 2 ^ 256 - 2 ^ 32 - 977

n :: Integer
n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

g :: Point Secp256k1Elem
g =
  secp256k1Point
    0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
    0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8

type Secp256k1Elem = Element

secp256k1Elem :: Integer -> Secp256k1Elem
secp256k1Elem num = Element num p

secp256k1Point :: Integer -> Integer -> Point Secp256k1Elem
secp256k1Point x y =
  Finite
    (secp256k1Elem x)
    (secp256k1Elem y)
    (secp256k1Elem a)
    (secp256k1Elem b)

verify :: Integer -> Signature -> Point Secp256k1Elem -> Bool
verify z sig sec = do
  let sInv = modExp (s sig) (n - 2) n
  let u = z * sInv `mod` n
  let v = r sig * sInv `mod` n
  let total = scale g u `add` scale sec v
  case total of
    Infinite _ _ -> False
    Finite (Element x _) _ _ _ -> x == r sig

data PrivateKey = PrivateKey {secret :: Integer, point :: Point Secp256k1Elem}

privateKey :: Integer -> PrivateKey
privateKey secret =
  PrivateKey secret (scale g secret)

sign :: PrivateKey -> Integer -> Signature
sign (PrivateKey secret _) z = do
  -- TODO: k must be unique for each signature
  let k = 49371293740912739421372438902
  let r = case scale g k of
        Finite (Element x _) _ _ _ -> x
        Infinite {} -> undefined
  let kInv = modExp k (n - 2) n
  let s = (z + r * secret) * kInv `mod` n
  if s > n `div` 2 then Signature r (n - s) else Signature r s
