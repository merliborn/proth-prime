module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest ["src/Math/Elliptic/Singular/Safe.hs"]