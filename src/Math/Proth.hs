{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

{-|
  Module      : Math.Proth
  Description : 
  Copyroght   : (c) merliborn 2021
  License     : MIT

  See [Deterministic Primality Proving on Proth Numbers](https://arxiv.org/abs/0812.2596) by Tsz-Wo Sze (arXiv:0812.2596 [math.NT]).
-}

module Math.Proth (
  -- * Main functions
--  pptest,
  sqRootOfMinusOne,
  sqRootWithQRoot,
) where

import GHC.Base ( (.) )
import GHC.Err (error, undefined)
import Data.Eq (Eq(..))
import Data.Ord (Ord(..))
import Data.Bool                 -- data Bool
    ( Bool(..), otherwise, (||) )
import Data.Maybe (Maybe(..))    -- data Maybe
import Data.Either (Either(..))  -- data Either
import System.IO                 -- data IO
    ( IO(..) )

import GHC.Num (Num(..))
import Numeric.Natural (Natural)
import GHC.Real                  -- class Integral
    ( Integral(..), (^), even )

import Control.Monad (Monad(..))
import Control.Exception.Safe    -- safe-exceptions (0.1.7.2 in lts-18.11)
    ( Exception(..),

      MonadThrow(..),
      MonadCatch(..),
      throw,            -- :: (MonadThrow m,Exception e) => e -> m a
      throwString       -- :: (MonadThrow m,HasCallStack) => String -> m a
     )

import Math.Elliptic.Singular.Safe
    ( SingularECPoint(..),
      infty,
      zero,
      toSingularECPoint,
      asIntegral,

      SingularECException(..),
       )
import Control.Partial 
  ( 
    Partially(..), 
    PartFunc(..), 
    PartFuncFail(..)
    )
import Data.Semigroup (Semigroup(..))
import Data.Group(Group(..))

-- ****************************
-- Proth test
-- ****************************
{-|
  Plain Proth test.
-}
pptest :: (Integral a) => 
  a->   --^ coefficient should be odd
  a->   --^ index
  Bool
pptest = undefined

-- ****************************
-- Auxiliary argorithms
-- ****************************
{-|
  Computing \(\sqrt{ -1}\mod N\) (Argorithm 2.2, [Sze18])

  [Inputs] @c :: Natural@ and @e :: (Integral a =>) a@
  such that modulus \(N\) is \(N=c\cdot 2^e+1\) for \(e>1\) and odd \(c\).

  [Output] @Nothing@ or @Just b@ such that @b :: Natural@ and \(b<N\)
-}

sqRootOfMinusOne :: (Integral a) => Natural-> a-> Maybe Natural
sqRootOfMinusOne c e = if (e <= 1) || even c then error "sqRootOfMinusOne: Invalid argument"
                       else fence (c * 2^ toNatural e) (2*c+1) 1
                          where
                            fence n l i
                              | i > l                = Nothing
                              | i^(l-1) `mod` n == 1 = fence n l (i+1)
                              | otherwise            = sqSearch n (e-2) (i^ toNatural e `mod` n) 0
                            sqSearch n l b i
                              | i > l              = Nothing
                              | b^2 `mod` n == n-1 = Just b
                              | otherwise          = sqSearch n l (b^2) (i+1)

{-|
  (Argorithm 3.4, [Sze18])

  [Inputs] @c :: Natural, e :: (Integral a=>) a, q :: Natural, b :: Natural@ such that
  modulus \(N\) is \(N=c\cdot 2^e\) for \(e>1\) and odd \(c\),
  \(1\leq b\leq N-1\) (but expected \(1<b<N-1\)) and \(q^2=-1\mod N\)

  [Output] \(\alpha\) such that \(\alpha^2=b\mod N\) if \(N\) is prime or algorithm find before halts.
-}
sqRootWithQRoot :: (Integral a, MonadThrow m) => Natural-> a-> Natural-> Natural-> m Natural
sqRootWithQRoot c e q b
  | (e<=1) || even c || b<1 || md<=b = throwString "sqRootWithQRoot: Invalid argument"
  | b == 1                           = return 1
  | b+1 == md                        = return q
  | (q^2 + 1) `mod` md /= 0          = throwString "sqRootWithQRoot: argument q must be a square root of -1 mod c*2^e."
  | otherwise
    = case sqCheck md (2*c+1) b 1 of
        Just n  -> return n
        Nothing -> do
          case findQuarticPoint md b c e of
            Fail e -> throw e
            Pass p -> case asIntegral p of
              Nothing -> throw NotPrimeModulus
              Just i ->
                let cand = (toNatural i * q) `mod` md in
                if ((cand^2) `mod` md) == b then return cand else throw NotPrimeModulus
      where
        md = c * 2^ toNatural e
        sqCheck :: Natural-> Natural-> Natural-> Natural-> Maybe Natural
        sqCheck mds max prm j
          | j > max   = Nothing
          | otherwise = if j^2 `mod` mds == prm then Just j
                          else sqCheck mds max prm (j+1)
        findQuarticPoint mds prm k i
          = case chk1 mds prm (2*k+1) (2*k) 1 of
            Nothing    -> throw NotPrimeModulus
            Just (a,p) -> case chk2 p (i-2) 0 of
              Fail f -> throw f
              Pass b -> pow (toSingularECPoint mds prm a) (k*2^b)
        chk1 mds prm max e j = do
          let p = toSingularECPoint mds prm j :: Partially (SingularECPoint Natural)
          let c = pow p e
          if c /= Pass infty then Just (j,c) else chk1 mds prm max e (j+1)
        chk2 :: Integral i => Partially (SingularECPoint Natural)-> i -> i -> Partially Natural
        chk2 x max k
          = case toEither x of
            Left e  -> fail e
            Right p -> chk2' p max k
            where
              chk2' p max k
                | k > max   = throw NotPrimeModulus
                | p == zr   = Pass (toNatural k)
                | otherwise = chk2 (x <> x) max (k+1)
        zr = zero md b

-- ****************************
-- Misc. functions
-- ****************************
toNatural :: (Integral a) => a-> Natural
toNatural = fromInteger . toInteger
