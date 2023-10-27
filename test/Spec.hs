import Control.Monad (void)
import Test.HUnit
import TestEllipticCurve
import TestFiniteField

main :: IO ()
main = void $ runTestTT allTests

allTests :: Test
allTests =
  TestList
    [ TestLabel "Test FiniteField" TestFiniteField.tests,
      TestLabel "Test EllipticCurve" TestEllipticCurve.tests
    ]
