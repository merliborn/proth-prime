{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
  Module      : Math.Elliptic.Singular
  Description :
  Copyright   : (c) merliborn 2021
  License     : MIT

  The "safe" version using [Control.Exception.Safe](https://hackage.haskell.org/package/safe-exceptions-0.1.7.2/docs/Control-Exception-Safe.html)
  
  See [Deterministic Primality Proving on Proth Numbers](https://arxiv.org/abs/0812.2596) by Tsz-Wo Sze (arXiv:0812.2596 [math.NT]).
-}

module Math.Elliptic.Singular.Safe(
  -- * Data
  SingularECPoint,
  infty,
  zero,

  -- ** Conversion
  toSingularECPoint,
  asIntegral,

  -- ** Exceptions
  SingularECException(..),
  invalidValueException,
  nonpositiveModException,
  notPrimeModException,
  differentTypeException
) where

-- partial-operations ^>= 0.1.0.0
import Control.Partial
  (
    Partially(..),
    PrePartial(..),
    PartFunc(..),
    PartFuncFail(..)
    )
import Data.Semigroup.Partial(PartialSemigroup(..))
import Data.Monoid.Partial(PartialMonoid(..))
import Data.Group.Partial (PartialGroup(..))

import GHC.Num (Num(..))
import GHC.Real     -- class Integral
    ( Integral(..), (^), gcd )
import Numeric.Natural (Natural)

import Data.Eq (Eq(..))
import Data.Ord (Ord(..))
import Data.Bool
import Data.Tuple (fst)
import Data.String (unwords)
import GHC.Base ( ($), (.) )
-- import GHC.Err (undefined)

import Control.Monad (Monad(..))
import Text.Show (Show(..),showString)
import Data.Maybe (Maybe(..))
import Control.Exception.Safe -- safe-exceptions
    ( Exception(..),
      SomeException(..),
      throw,

      MonadThrow(..)
    )

{-
  Non-singular points on the singular elliptic curve
      y^2 = x^2 * (x + a^2)
  represented as [z] (in Z/NZ) s.t. z != a,-a (mod N) or [infty] (the infinity point)
  (where N should be a prime)

  x != 0 (because (0,0) is singular point).
  (x,y) |--> y/x
  [z]   |--> (z^2-a^2, z^3-a^2z)

  See "Deterministic Primality Proving on Proth Numbers" by Tsz-Wo Sze (arXiv:0812.2596 [math.NT]).
-}
-- | A point on singular curve \(y^2 = x^2(x+\alpha) \) over \(\mathbb{Z}/{N\mathbb{Z}}\cup\{\infty\}\)
-- 
-- The value of type 'SingularECPoint' can be constructed by using 'infty' or 'toSingularECPoint'.
data SingularECPoint i where
  Pt    :: Integral i => Natural-> i-> i-> SingularECPoint i
  Infty :: SingularECPoint i

instance PrePartial SingularECPoint where
  Pt m0 b0 x0 `eq` Pt m1 b1 x1 = m0 == m1 && b0 == b1 && x0 == x1
  Pt {}       `eq` Infty       = False
  Infty       `eq` Pt {}       = False
  Infty       `eq` Infty       = True

instance Show i => Show (SingularECPoint i) where
  showsPrec _ Infty        = showString "Infty"
  showsPrec _ (Pt md pr i) = showString (unwords ["Pt", show md, show pr, show i])

-- | The infinity point / The unit of group operation @\<?\>@.
infty :: Integral i => SingularECPoint i
infty = Infty

zero :: Integral i => Natural-> i-> SingularECPoint i
zero md pr = Pt md pr 0

-- ------------------------------------------------------------------------
-- Exceptions
data SingularECException
  = InvalidValue          -- ^ Invalid value. Values should be not equal @prm@ nor -@prm@ (mod @mds@).
  | NonpositiveModulus    -- ^ Modulus @mds@ should be a positive, not zero nor negative.
  | NotPrimeModulus       -- ^ Modulus @mds@ should be a prime, not composite.
  | DifferentType         -- ^ Two argument must share @mds@ and @prm@ both.
  deriving (Eq)

invalidValueException, nonpositiveModException, notPrimeModException, differentTypeException :: SomeException
invalidValueException   = toException InvalidValue
nonpositiveModException = toException NonpositiveModulus
notPrimeModException    = toException NotPrimeModulus
differentTypeException  = toException DifferentType

instance Exception SingularECException
instance Show SingularECException where
  showsPrec _ InvalidValue       = showString "invalid value/parameter"
  showsPrec _ NonpositiveModulus = showString "nonpositive modulus"
  showsPrec _ NotPrimeModulus    = showString "modulus is composite"
  showsPrec _ DifferentType      = showString "Arguments must share modulus and parameter"
-- End Exceptions
-- ------------------------------------------------------------------------
-- Conversion
-- ------------------------------------------------------------------------
-- |
--  Convert integers into points in a singular curve.
-- For @toSingularECPoint N b x@, if \(N\leq 0\), \(b\leq 0\), \(b\geq N\) or \(x=\pm b\mod N\),
-- then it throws 'InvalidValue' exception.
toSingularECPoint :: (Integral i, MonadThrow m) => Natural-> i-> i -> m (SingularECPoint i)
toSingularECPoint md b i
  | md <= 0              = throw InvalidValue
  | b <= 0 || b >= mi    = throw InvalidValue
  | i `mod` mi == b      = throw InvalidValue
  | i `mod` mi == mi - b = throw InvalidValue
  | otherwise            = return (Pt md b i)
  where
    mi = fromInteger $ toInteger md

-- |
-- Cast into base 'Integral' type.
--
-- >>> asIntegral (Pt 5 1 2)
-- Just 2
--
-- >>> asIntegral Infty
-- Nothing
asIntegral :: (Integral i) => SingularECPoint i -> Maybe i
asIntegral Infty          = Nothing
asIntegral (Pt mds prm i) = Just i

-- ------------------------------------------------------------------------
-- Operations
-- |
-- This operation throws 'NotPrimeModulus' if
-- 
-- 1. \(\gcd(x+y,N)\neq 1\), or
-- 
-- 2. \(((xy+\alpha)(x+y)^{-1})^2=\alpha\mod N\)
-- 
-- because \(N\) is composite in these cases.
instance Integral i => PartialSemigroup (SingularECPoint i) where
  Infty        <?> q            = return q
  p            <?> Infty        = return p
  (Pt m0 b0 x) <?> (Pt m1 b1 y)
    | m0 /= m1  = throw DifferentType
    | b0 /= b1  = throw DifferentType
    | (x+y) `mod` md == 0 = return Infty
    | gcd (x+y) md /= 1   = throw NotPrimeModulus
    | otherwise           = case modInv m0 (x+y) of
                              Nothing -> throw NotPrimeModulus
                              Just rc -> let r = (x * y + b0) * rc `mod` md in
                                if r^2 `mod` md == b0  then throw NotPrimeModulus
                                  else return (Pt m0 b0 r)
    where md = fromInteger $ toInteger m0

-- |
-- The unit of the operation for @\<?\>@ (= the infinity point \(\infty\)).
instance  Integral i => PartialMonoid (SingularECPoint i) where
  munit = Infty

-- |
-- The invertion operation for @\<?\>@.
--
-- This operation throws 'InvalidValue' if 
-- the argument \(x\) is invalid (i.e. \(x=\pm\alpha\mod N\)).
instance  Integral i => PartialGroup (SingularECPoint i) where
  inv Infty = return Infty
  inv (Pt md prm x)
    | x' == prm       = throw InvalidValue
    | x' == mds - prm = throw InvalidValue
    | otherwise       = return (Pt md prm (-x `mod` mds))
    where
      mds = fromInteger $ toInteger md
      x' = x `mod` mds

-- ------------------------------------------------------------------------
-- Private
-- ------------------------------------------------------------------------
modInv :: Integral i => Natural-> i-> Maybe i
modInv md i =
  case xea (fromInteger $ toInteger md) i (1,0) of
    Nothing    -> Nothing
    Just (x,u) -> Just x
  where
    xea m n (x, u)
      | m == 0 = Nothing
      | n == 0
      = if m == 1 then Just (m,n) else Nothing
      | otherwise
      = let (k,r) = divMod m n in xea n r (u,x-k*u)