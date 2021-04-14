{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |
-- Module    : Numeric.Sum
-- Copyright : (c) 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for summing floating point numbers more accurately than
-- the naive 'Prelude.sum' function and its counterparts in the
-- @vector@ package and elsewhere.
--
-- When used with floating point numbers, in the worst case, the
-- 'Prelude.sum' function accumulates numeric error at a rate
-- proportional to the number of values being summed. The algorithms
-- in this module implement different methods of /compensated
-- summation/, which reduce the accumulation of numeric error so that
-- it either grows much more slowly than the number of inputs
-- (e.g. logarithmically), or remains constant.
module Numeric.Sum (
    -- * Summation type class
      Summation(..)
    , sumVector
    -- ** Usage
    -- $usage

    -- * Kahan-Babuška-Neumaier summation
    , KBNSum(..)
    , kbn

    -- * Order-2 Kahan-Babuška summation
    , KB2Sum(..)
    , kb2

    -- * Less desirable approaches

    -- ** Kahan summation
    , KahanSum(..)
    , kahan

    -- ** Pairwise summation
    , pairwiseSum

    -- * References
    -- $references
    ) where

import Control.Arrow ((***))
import Control.DeepSeq (NFData(..))
import Data.Bits (shiftR)
import Data.Data (Typeable, Data)
import Data.Monoid                  (Monoid(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup               (Semigroup(..))
#endif
import Data.Vector.Generic          (Vector(..), foldl')
-- Needed for GHC 7.2 & 7.4 to derive Unbox instances
import Control.Monad (liftM)
import Data.Vector.Generic.Mutable (MVector(..))

import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

-- | A class for summation of floating point numbers.
class RealFloat a => Summation s a where
    -- | The identity for summation.
    zero :: s a

    -- | Add a value to a sum.
    add  :: s a -> a -> s a

    -- | Sum a collection of values.
    --
    -- Example:
    -- @foo = 'Numeric.Sum.sum' 'kbn' [1,2,3]@
    sum :: F.Foldable f => (s a -> a) -> f a -> a
    sum f = f . F.foldl' add (zero :: s a)
    {-# INLINE sum #-}

-- | Kahan summation. This is the least accurate of the compensated
-- summation methods.  In practice, it only beats naive summation for
-- inputs with large magnitude.  Kahan summation can be /less/
-- accurate than naive summation for small-magnitude inputs.
--
-- This summation method is included for completeness. Its use is not
-- recommended.  In practice, 'KBNSum' is both 30% faster and more
-- accurate.
data KahanSum a = KahanSum !a !a
              deriving (Eq, Show, Typeable, Data)

instance U.Unbox a => U.Unbox (KahanSum a)
newtype instance U.MVector s (KahanSum a) = MV_KahanSum (U.MVector s (a, a))
instance U.Unbox a => MVector U.MVector (KahanSum a) where
  {-# INLINE GM.basicLength #-}
  {-# INLINE GM.basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE GM.basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_KahanSum mvec) = GM.basicLength mvec
  basicUnsafeSlice idx len (MV_KahanSum mvec) = MV_KahanSum (GM.basicUnsafeSlice idx len mvec)
  basicOverlaps (MV_KahanSum mvec) (MV_KahanSum mvec') = basicOverlaps mvec mvec'
  basicUnsafeNew len = MV_KahanSum `liftM` basicUnsafeNew len
  basicInitialize (MV_KahanSum mvec) = basicInitialize mvec
  basicUnsafeReplicate len val = MV_KahanSum `liftM` basicUnsafeReplicate len ((\ (KahanSum a b) -> (a, b)) val)
  basicUnsafeRead (MV_KahanSum mvec) idx = (\ (a, b) -> KahanSum a b) `liftM` basicUnsafeRead mvec idx
  basicUnsafeWrite (MV_KahanSum mvec) idx val = basicUnsafeWrite mvec idx ((\ (KahanSum a b) -> (a, b)) val)
  basicClear (MV_KahanSum mvec) = basicClear mvec
  basicSet (MV_KahanSum mvec) val = basicSet mvec ((\ (KahanSum a b) -> (a, b)) val)
  basicUnsafeCopy (MV_KahanSum mvec) (MV_KahanSum mvec') = GM.basicUnsafeCopy mvec mvec'
  basicUnsafeMove (MV_KahanSum mvec) (MV_KahanSum mvec') = basicUnsafeMove mvec mvec'
  basicUnsafeGrow (MV_KahanSum mvec) len = MV_KahanSum `liftM` basicUnsafeGrow mvec len

newtype instance U.Vector (KahanSum a) = V_KahanSum (U.Vector (a, a))
instance U.Unbox a => Vector U.Vector (KahanSum a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE G.basicLength #-}
  {-# INLINE G.basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE G.basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_KahanSum mvec) = V_KahanSum `liftM` basicUnsafeFreeze mvec
  basicUnsafeThaw (V_KahanSum vec) = MV_KahanSum `liftM` basicUnsafeThaw vec
  basicLength (V_KahanSum vec) = G.basicLength vec
  basicUnsafeSlice idx len (V_KahanSum vec) = V_KahanSum (G.basicUnsafeSlice idx len vec)
  basicUnsafeIndexM (V_KahanSum vec) idx = (\ (a, b) -> KahanSum a b) `liftM` basicUnsafeIndexM vec idx
  basicUnsafeCopy (MV_KahanSum mvec) (V_KahanSum vec) = G.basicUnsafeCopy mvec vec
  elemseq (V_KahanSum vec) val = elemseq vec ((\ (KahanSum a b) -> (a, b)) val)


instance RealFloat a => Summation KahanSum a where
    zero = KahanSum 0 0
    add  = kahanAdd

instance NFData (KahanSum a) where
    rnf !_ = ()

-- | @since 0.3.0.0
instance RealFloat a => Monoid (KahanSum a) where
  mempty = zero
  s `mappend` KahanSum s' _ = add s s'

#if MIN_VERSION_base(4,9,0)
-- | @since 0.3.0.0
instance RealFloat a => Semigroup (KahanSum a) where
  (<>) = mappend
#endif

kahanAdd :: RealFloat a => KahanSum a -> a -> KahanSum a
kahanAdd (KahanSum sum c) x = KahanSum sum' c'
  where sum' = sum + y
        c'   = (sum' - sum) - y
        y    = x - c

-- | Return the result of a Kahan sum.
kahan :: KahanSum a -> a
kahan (KahanSum sum _) = sum

-- | Kahan-Babuška-Neumaier summation. This is a little more
-- computationally costly than plain Kahan summation, but is /always/
-- at least as accurate.
data KBNSum a = KBNSum !a !a
            deriving (Eq, Show, Typeable, Data)

instance U.Unbox a => U.Unbox (KBNSum a)
newtype instance U.MVector s (KBNSum a) = MV_KBNSum (U.MVector s (a, a))
instance U.Unbox a => MVector U.MVector (KBNSum a) where
  {-# INLINE GM.basicLength #-}
  {-# INLINE GM.basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE GM.basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_KBNSum mvec) = GM.basicLength mvec
  basicUnsafeSlice idx len (MV_KBNSum mvec) = MV_KBNSum (GM.basicUnsafeSlice idx len mvec)
  basicOverlaps (MV_KBNSum mvec) (MV_KBNSum mvec') = basicOverlaps mvec mvec'
  basicUnsafeNew len = MV_KBNSum `liftM` basicUnsafeNew len
  basicInitialize (MV_KBNSum mvec) = basicInitialize mvec
  basicUnsafeReplicate len val = MV_KBNSum `liftM` basicUnsafeReplicate len ((\ (KBNSum a b) -> (a, b)) val)
  basicUnsafeRead (MV_KBNSum mvec) idx = (\ (a, b) -> KBNSum a b) `liftM` basicUnsafeRead mvec idx
  basicUnsafeWrite (MV_KBNSum mvec) idx val = basicUnsafeWrite mvec idx ((\ (KBNSum a b) -> (a, b)) val)
  basicClear (MV_KBNSum mvec) = basicClear mvec
  basicSet (MV_KBNSum mvec) val = basicSet mvec ((\ (KBNSum a b) -> (a, b)) val)
  basicUnsafeCopy (MV_KBNSum mvec) (MV_KBNSum mvec') = GM.basicUnsafeCopy mvec mvec'
  basicUnsafeMove (MV_KBNSum mvec) (MV_KBNSum mvec') = basicUnsafeMove mvec mvec'
  basicUnsafeGrow (MV_KBNSum mvec) len = MV_KBNSum `liftM` basicUnsafeGrow mvec len

newtype instance U.Vector (KBNSum a) = V_KBNSum (U.Vector (a, a))
instance U.Unbox a => Vector U.Vector (KBNSum a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE G.basicLength #-}
  {-# INLINE G.basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE G.basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_KBNSum mvec) = V_KBNSum `liftM` basicUnsafeFreeze mvec
  basicUnsafeThaw (V_KBNSum vec) = MV_KBNSum `liftM` basicUnsafeThaw vec
  basicLength (V_KBNSum vec) = G.basicLength vec
  basicUnsafeSlice idx len (V_KBNSum vec) = V_KBNSum (G.basicUnsafeSlice idx len vec)
  basicUnsafeIndexM (V_KBNSum vec) idx = (\ (a, b) -> KBNSum a b) `liftM` basicUnsafeIndexM vec idx
  basicUnsafeCopy (MV_KBNSum mvec) (V_KBNSum vec) = G.basicUnsafeCopy mvec vec
  elemseq (V_KBNSum vec) val = elemseq vec ((\ (KBNSum a b) -> (a, b)) val)


instance RealFloat a => Summation KBNSum a where
    zero = KBNSum 0 0
    add  = kbnAdd

instance NFData (KBNSum a) where
    rnf !_ = ()

-- | @since 0.3.0.0
instance RealFloat a => Monoid (KBNSum a) where
  mempty = zero
  s `mappend` KBNSum s' c' = add (add s s') c'

#if MIN_VERSION_base(4,9,0)
-- | @since 0.3.0.0
instance RealFloat a => Semigroup (KBNSum a) where
  (<>) = mappend
#endif

kbnAdd :: (Num a, Ord a) => KBNSum a -> a -> KBNSum a
kbnAdd (KBNSum sum c) x = KBNSum sum' c'
  where c' | abs sum >= abs x = c + ((sum - sum') + x)
           | otherwise        = c + ((x - sum') + sum)
        sum'                  = sum + x

-- | Return the result of a Kahan-Babuška-Neumaier sum.
kbn :: Num a => KBNSum a -> a
kbn (KBNSum sum c) = sum + c

-- | Second-order Kahan-Babuška summation.  This is more
-- computationally costly than Kahan-Babuška-Neumaier summation,
-- running at about a third the speed.  Its advantage is that it can
-- lose less precision (in admittedly obscure cases).
--
-- This method compensates for error in both the sum and the
-- first-order compensation term, hence the use of \"second order\" in
-- the name.
data KB2Sum a = KB2Sum !a !a !a
            deriving (Eq, Show, Typeable, Data)

instance U.Unbox a => U.Unbox (KB2Sum a)
newtype instance U.MVector s (KB2Sum a) = MV_KB2Sum (U.MVector s (a, a, a))
instance U.Unbox a => MVector U.MVector (KB2Sum a) where
  {-# INLINE GM.basicLength #-}
  {-# INLINE GM.basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE GM.basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_KB2Sum mvec) = GM.basicLength mvec
  basicUnsafeSlice idx len (MV_KB2Sum mvec) = MV_KB2Sum (GM.basicUnsafeSlice idx len mvec)
  basicOverlaps (MV_KB2Sum mvec) (MV_KB2Sum mvec') = basicOverlaps mvec mvec'
  basicUnsafeNew len = MV_KB2Sum `liftM` basicUnsafeNew len
  basicInitialize (MV_KB2Sum mvec) = basicInitialize mvec
  basicUnsafeReplicate len val = MV_KB2Sum `liftM` basicUnsafeReplicate len ((\ (KB2Sum a b c) -> (a, b, c)) val)
  basicUnsafeRead (MV_KB2Sum mvec) idx = (\ (a, b, c) -> KB2Sum a b c) `liftM` basicUnsafeRead mvec idx
  basicUnsafeWrite (MV_KB2Sum mvec) idx val = basicUnsafeWrite mvec idx ((\ (KB2Sum a b c) -> (a, b, c)) val)
  basicClear (MV_KB2Sum mvec) = basicClear mvec
  basicSet (MV_KB2Sum mvec) val = basicSet mvec ((\ (KB2Sum a b c) -> (a, b, c)) val)
  basicUnsafeCopy (MV_KB2Sum mvec) (MV_KB2Sum mvec') = GM.basicUnsafeCopy mvec mvec'
  basicUnsafeMove (MV_KB2Sum mvec) (MV_KB2Sum mvec') = basicUnsafeMove mvec mvec'
  basicUnsafeGrow (MV_KB2Sum mvec) len = MV_KB2Sum `liftM` basicUnsafeGrow mvec len

newtype instance U.Vector (KB2Sum a) = V_KB2Sum (U.Vector (a, a, a))
instance U.Unbox a => Vector U.Vector (KB2Sum a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE G.basicLength #-}
  {-# INLINE G.basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE G.basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_KB2Sum mvec) = V_KB2Sum `liftM` basicUnsafeFreeze mvec
  basicUnsafeThaw (V_KB2Sum vec) = MV_KB2Sum `liftM` basicUnsafeThaw vec
  basicLength (V_KB2Sum vec) = G.basicLength vec
  basicUnsafeSlice idx len (V_KB2Sum vec) = V_KB2Sum (G.basicUnsafeSlice idx len vec)
  basicUnsafeIndexM (V_KB2Sum vec) idx = (\ (a, b, c) -> KB2Sum a b c) `liftM` basicUnsafeIndexM vec idx
  basicUnsafeCopy (MV_KB2Sum mvec) (V_KB2Sum vec) = G.basicUnsafeCopy mvec vec
  elemseq (V_KB2Sum vec) val = elemseq vec ((\ (KB2Sum a b c) -> (a, b, c)) val)

instance RealFloat a => Summation KB2Sum a where
    zero = KB2Sum 0 0 0
    add  = kb2Add

instance NFData (KB2Sum a) where
    rnf !_ = ()

-- | @since 0.3.0.0
instance RealFloat a => Monoid (KB2Sum a) where
  mempty = zero
  s `mappend` KB2Sum s' c' cc' = add (add (add s s') c') cc'

#if MIN_VERSION_base(4,9,0)
-- | @since 0.3.0.0
instance RealFloat a => Semigroup (KB2Sum a) where
  (<>) = mappend
#endif


kb2Add :: (Num a, Ord a) => KB2Sum a -> a -> KB2Sum a
kb2Add (KB2Sum sum c cc) x = KB2Sum sum' c' cc'
  where sum'                 = sum + x
        c'                   = c + k
        cc' | abs c >= abs k = cc + ((c - c') + k)
            | otherwise      = cc + ((k - c') + c)
        k | abs sum >= abs x = (sum - sum') + x
          | otherwise        = (x - sum') + sum

-- | Return the result of an order-2 Kahan-Babuška sum.
kb2 :: Num a => KB2Sum a -> a
kb2 (KB2Sum sum c cc) = sum + c + cc

-- | /O(n)/ Sum a vector of values.
sumVector :: RealFloat a => (Vector v a, Summation s a) => (s a -> a) -> v a -> a
sumVector f = f . foldl' add zero
{-# INLINE sumVector #-}

-- | /O(n)/ Sum a vector of values using pairwise summation.
--
-- This approach is perhaps 10% faster than 'KBNSum', but has poorer
-- bounds on its error growth.  Instead of having roughly constant
-- error regardless of the size of the input vector, in the worst case
-- its accumulated error grows with /O(log n)/.
pairwiseSum :: RealFloat a => (Vector v a) => v a -> a
pairwiseSum v
  | len <= 256 = G.sum v
  | otherwise  = uncurry (+) . (pairwiseSum *** pairwiseSum) .
                 G.splitAt (len `shiftR` 1) $ v
  where len = G.length v
{-# SPECIALIZE pairwiseSum :: V.Vector Double -> Double #-}
{-# SPECIALIZE pairwiseSum :: U.Vector Double -> Double #-}

-- $usage
--
-- Most of these summation algorithms are intended to be used via the
-- 'Summation' typeclass interface. Explicit type annotations should
-- not be necessary, as the use of a function such as 'kbn' or 'kb2'
-- to extract the final sum out of a 'Summation' instance gives the
-- compiler enough information to determine the precise type of
-- summation algorithm to use.
--
-- As an example, here is a (somewhat silly) function that manually
-- computes the sum of elements in a list.
--
-- @
-- sillySumList :: RealFloat a => [a] -> a
-- sillySumList = loop 'zero'
--   where loop s []     = 'kbn' s
--         loop s (x:xs) = 'seq' s' loop s' xs
--           where s'    = 'add' s x
-- @
--
-- In most instances, you can simply use the much more general 'Numeric.Sum.sum'
-- function instead of writing a summation function by hand.
--
-- @
-- -- Avoid ambiguity around which sum function we are using.
-- import Prelude hiding (sum)
-- --
-- betterSumList :: RealFloat a => [a] -> a
-- betterSumList xs = 'Numeric.Sum.sum' 'kbn' xs
-- @

-- Note well the use of 'seq' in the example above to force the
-- evaluation of intermediate values.  If you must write a summation
-- function by hand, and you forget to evaluate the intermediate
-- values, you are likely to incur a space leak.
--
-- Here is an example of how to compute a prefix sum in which the
-- intermediate values are as accurate as possible.
--
-- @
-- prefixSum :: RealFloat a => [a] -> [a]
-- prefixSum xs = map 'kbn' . 'scanl' 'add' 'zero' $ xs
-- @

-- $references
--
-- * Kahan, W. (1965), Further remarks on reducing truncation
--   errors. /Communications of the ACM/ 8(1):40.
--
-- * Neumaier, A. (1974), Rundungsfehleranalyse einiger Verfahren zur
--   Summation endlicher Summen.
--   /Zeitschrift für Angewandte Mathematik und Mechanik/ 54:39–51.
--
-- * Klein, A. (2006), A Generalized
--   Kahan-Babuška-Summation-Algorithm. /Computing/ 76(3):279-293.
--
-- * Higham, N.J. (1993), The accuracy of floating point
--   summation. /SIAM Journal on Scientific Computing/ 14(4):783–799.
