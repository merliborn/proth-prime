{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
  Module      : Math.Elliptic.Singular
  Description :
  Copyright   : (c) merliborn 2021
  License     : MIT

  See [Deterministic Primality Proving on Proth Numbers](https://arxiv.org/abs/0812.2596) by Tsz-Wo Sze (arXiv:0812.2596 [math.NT]).
-}

module Math.Elliptic.Singular(
  -- * Data
  SingularECPoint,
  infty,

  -- ** Conversion
  toSingularECPoint,
  asIntegral,

  -- ** Exceptions
  SingularECException(..),
  invalidValueException,
  nonpositiveModException,
  notPrimeModException,
  differentTypeException,
) where

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

import Text.Show (Show(..),showString)
import Data.Maybe (Maybe(..))
import Control.Exception
    ( Exception(..),
      SomeException(..),

      throw )

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.Group (Group(..))
import Prelude (Maybe(Nothing))

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
-- with a group operation @<>@.
-- 
-- The value of type 'SingularECPoint' can be constructed by using 'infty' or 'toSingularECPoint'.
data SingularECPoint i where
  Pt    :: Integral i => Natural-> i-> i-> SingularECPoint i
  Infty :: SingularECPoint i

instance Eq (SingularECPoint i) where
  Infty        == Infty        = True
  Infty        == (Pt md pr i) = False
  (Pt md pr i) == Infty        = False
  (Pt m0 b0 x) == (Pt m1 b1 y) = m0 == m1 && b0 == b1 && x == y

instance Show i => Show (SingularECPoint i) where
  showsPrec _ Infty        = showString "Infty"
  showsPrec _ (Pt md pr i) = showString (unwords ["Pt",show md, show pr, show i])

-- | The infinity point / The unit of group operation @<>@.
infty :: SingularECPoint i
infty = Infty

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
toSingularECPoint :: (Integral i) => Natural-> i-> i -> SingularECPoint i
toSingularECPoint md b i
  | md <= 0              = throw InvalidValue
  | b <= 0 || b >= mi    = throw InvalidValue
  | i `mod` mi == b      = throw InvalidValue
  | i `mod` mi == mi - b = throw InvalidValue
  | otherwise            = Pt md b i
  where
    mi = fromInteger $ toInteger md

-- |
-- Cast into base 'Integral' type.
asIntegral :: (Integral i) => SingularECPoint i -> Maybe i
asIntegral Infty  = Nothing
asIntegral (Pt mds prm i) = Just i

-- ------------------------------------------------------------------------
-- Semigroup/Monoid/Group operations

-- |
-- If @m0 /= m1@ or @b0 /= b1@ in @(Pt m0 b0 x) <> (Pt m1 b1 y)@,
-- throw 'DifferentType' exception.
instance (Integral i) => Semigroup (SingularECPoint i) where
  Infty        <> q            = q
  p            <> Infty        = p
  (Pt m0 b0 x) <> (Pt m1 b1 y)
    | m0 /= m1  = throw DifferentType
    | b0 /= b1  = throw DifferentType
    | (x+y) `mod` md == 0 = Infty
    | gcd (x+y) md /= 1   = throw NotPrimeModulus
    | otherwise           = case modInv m0 (x+y) of
                              Nothing -> throw NotPrimeModulus
                              Just rc -> let r = (x * y + bb) * rc `mod` md in
                                if r^2 `mod` md == bb  then throw NotPrimeModulus
                                  else Pt m0 b0 r
    where md = fromInteger $ toInteger m0
          bb = b0 * b1 `mod` md

instance (Integral i) => Monoid (SingularECPoint i) where
  mempty = Infty
instance (Integral i) => Group (SingularECPoint i) where
  invert Infty  = Infty
  invert (Pt md b x)
    | x' == b      = throw InvalidValue
    | x' == mi - b = throw InvalidValue
    | otherwise    = Pt md b (mi-x')
    where
      mi = fromInteger $ toInteger md
      x' = x `mod` mi
-- End Semigroup/Monoid/Group operations
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