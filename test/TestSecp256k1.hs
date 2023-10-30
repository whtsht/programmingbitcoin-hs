{-# OPTIONS_GHC -Wno-type-defaults #-}

module TestSecp256k1 (tests) where

import EllipticCurve (Point (Finite, Infinite), scale)
import Secp256k1 (PrivateKey (point), g, n, privateKey, secp256k1Point, sign, verify)
import Test.HUnit

tests :: Test
tests = TestList [TestCase testOrder, TestCase testPubPoint, TestCase testPrivateKey]

eq :: (Eq a, Show a) => a -> a -> Assertion
eq = assertEqual ""

testOrder :: Assertion
testOrder = do
  let p = scale g n
  case p of
    Finite {} -> error "G * N must be infinity"
    Infinite {} -> return ()

testPubPoint :: Assertion
testPubPoint = do
  let list =
        [ (7, 0x5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc, 0x6aebca40ba255960a3178d6d861a54dba813d0b813fde7b5a5082628087264da),
          (1485, 0xc982196a7466fbbbb0e27a940b6af926c1a74d5ad07128c82824a11b5398afda, 0x7a91f9eae64438afb9ce6448a1c133db2d8fb9254e4546b6f001637d50901f55),
          (2 ^ 128, 0x8f68b9d2f63b5f339239c1ad981f162ee88c5678723ea3351b7b444c9ec4c0da, 0x662a9f2dba063986de1d90c2b6be215dbbea2cfe95510bfdf23cbf79501fff82),
          (2 ^ 240 + 2 ^ 31, 0x9577ff57c8234558f293df502ca4f09cbc65a6572c842b39b366f21717945116, 0x10b49c67fa9365ad7b90dab070be339a1daf9052373ec30ffae4f72d5e66d053)
        ]
  mapM_ (\(secret, x, y) -> eq (scale g secret) (secp256k1Point x y)) list

testPrivateKey :: Assertion
testPrivateKey = do
  let secret = 100
  let pk = privateKey secret
  let z = 201397542
  let sig = sign pk z
  eq (verify z sig (point pk)) True
