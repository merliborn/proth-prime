module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Show (Show(..))
import Numeric.Natural(Natural)
import Data.Group (Group(..))

import Math.Elliptic.Singular
import qualified Math.Elliptic.Singular.Safe as S

main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Tests for Elliptic.Singular modules"
  [
    tEllipticSingular
  ]

tEllipticSingular :: TestTree
tEllipticSingular = testGroup "Tests for Elliptic.Singular (unsafe)"
                      [
                        tShowECPData,
                        tConvertECPData,
                        tGroupOpetarion
                      ]

tEllipticSingularSafe :: TestTree
tEllipticSingularSafe = testGroup "Tests for Elliptic.Singular.Safe"
                      [

                      ]

tShowECPData :: TestTree
tShowECPData = testGroup "Showing Data"
  [
    testCase "show infty" $ show (infty::SingularECPoint Natural) @=? "Infty",
    testCase "show (Pt md pr i)" $ show (toSingularECPoint 3 1 0) @=? "Pt 3 1 0"
  ]

tConvertECPData :: TestTree
tConvertECPData = testGroup "Converting SingularECPoint <--> Number"
  [
    testCase "Number --> SingularECPoint" $ ((toSingularECPoint 3 1 0) :: SingularECPoint Natural) @=? toSingularECPoint 3 1 0,
    testCase "SingularECPoint --> Number" (do
      asIntegral (infty :: SingularECPoint Natural) @=? Nothing
      asIntegral (toSingularECPoint 5 2 1) @=? Just 1
    )
  ]

tGroupOpetarion :: TestTree
tGroupOpetarion = testGroup "Group operations"
  [
    testCase "(Pt 5 2 1) <> (Pt 5 2 0) == (Pt 5 2 4)" ((toSingularECPoint 5 2 1) <> (toSingularECPoint 5 2 0) @=? toSingularECPoint 5 2 4),
    testCase "(Pt 5 2 1) <> Infty == (Pt 5 2 1)" ((toSingularECPoint 5 2 1) <> (infty :: SingularECPoint Natural) @=? toSingularECPoint 5 2 1),
    testCase "invert (Pt 5 2 1) == (Pt 5 2 4)" (invert (toSingularECPoint 5 2 1) @=? toSingularECPoint 5 2 4)
  ]
