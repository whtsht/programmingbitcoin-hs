import Control.Monad (void)
import Test.HUnit
import TestEllipticCurve
import TestFiniteField
import TestHash
import TestSecp256k1

main :: IO ()
main = void $ runTestTT allTests

allTests :: Test
allTests =
  TestList
    [ TestLabel "Test FiniteField" TestFiniteField.tests,
      TestLabel "Test EllipticCurve" TestEllipticCurve.tests,
      TestLabel "Test Hash" TestHash.tests,
      TestLabel "Test Secp256k1" TestSecp256k1.tests
    ]
