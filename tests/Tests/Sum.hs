{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Sum (tests) where

import Control.Applicative ((<$>))
import Foreign.C.Types
import Numeric.Sum as Sum
import Prelude hiding (sum)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Prelude

t_sum :: RealFloat a => ([a] -> a) -> [a] -> Bool
t_sum f xs = f xs == trueSum xs

t_sum_error :: RealFloat a => ([a] -> a) -> [a] -> Bool
t_sum_error f xs = abs (ts - f xs) <= abs (ts - Prelude.sum xs)
  where ts = trueSum xs

t_sum_shifted :: RealFloat a => ([a] -> a) -> [a] -> Bool
t_sum_shifted f = t_sum_error f . zipWith (+) badvec

trueSum :: (Fractional b, Real a) => [a] -> b
trueSum xs = fromRational . Prelude.sum . map toRational $ xs

badvec :: RealFloat a => [a]
badvec = cycle [1,1e16,-1e16]

tests :: TestTree
tests = testGroup "Summation" [
    testGroup "ID" [
      -- plain summation loses precision quickly
      -- testProperty "t_sum" $ t_sum (sum id)

      -- tautological tests:
      -- testProperty "t_sum_error" $ t_sum_error (sum id)
      -- testProperty "t_sum_shifted" $ t_sum_shifted (sum id)
    ]
  , testGroup "Kahan" $ testShifted kahan
      -- tests that cannot pass:
      -- testProperty "t_sum" $ t_sum (sum kahan)
      -- testProperty "t_sum_error" $ t_sum_error (sum kahan)

      -- kahan summation only beats normal summation with large values
  , testGroup "KBN" $ testSum kbn
  , testGroup "KB2" $ testSum kb2
  ]

type SummationTestTypes s =
  ( Summation s Float
  , Summation s Double
  , Summation s CFloat
  , Summation s CDouble
  )

testShifted :: forall s. SummationTestTypes s
            => (forall a. Summation s a => s a -> a)
            -> [TestTree]
testShifted f = testOnTypes f [ ("t_sum_shifted", t_sum_shifted) ]

testSum :: forall s. SummationTestTypes s
        => (forall a. Summation s a => s a -> a)
        -> [TestTree]
testSum f = testOnTypes f
  [ ("t_sum", t_sum)
  , ("t_sum_error", t_sum_error)
  , ("t_sum_shifted", t_sum_shifted)
  ]

testOnTypes :: forall s. SummationTestTypes s
            => (forall a. Summation s a => s a -> a)
            -> (forall a. Summation s a => [ (String, ([a] -> a) -> [a] -> Bool) ])
            -> [TestTree]
testOnTypes f ts =
  [ testGroup "Float" $ toTest (f :: s Float -> Float) <$> ts
  , testGroup "Double" $ toTest (f :: s Double -> Double) <$> ts
  , testGroup "CFloat" $ toTest (f :: s CFloat -> CFloat) <$> ts
  , testGroup "CDouble" $ toTest (f :: s CDouble -> CDouble) <$> ts
  ]
  where
    toTest f' (testName, test) = testProperty testName $ test (sum f')

instance Arbitrary a => Arbitrary (KahanSum a) where
    arbitrary = toKahan <$> arbitrary
    shrink = map toKahan . shrink . fromKahan

toKahan :: (a, a) -> KahanSum a
toKahan (a,b) = KahanSum a b

fromKahan :: KahanSum a -> (a, a)
fromKahan (KahanSum a b) = (a,b)

instance Arbitrary a => Arbitrary (KBNSum a) where
    arbitrary = toKBN <$> arbitrary
    shrink = map toKBN . shrink . fromKBN

toKBN :: (a, a) -> KBNSum a
toKBN (a,b) = KBNSum a b

fromKBN :: KBNSum a -> (a, a)
fromKBN (KBNSum a b) = (a,b)

instance Arbitrary a => Arbitrary (KB2Sum a) where
    arbitrary = toKB2 <$> arbitrary
    shrink = map toKB2 . shrink . fromKB2

toKB2 :: (a, a, a) -> KB2Sum a
toKB2 (a,b,c) = KB2Sum a b c

fromKB2 :: KB2Sum a -> (a, a, a)
fromKB2 (KB2Sum a b c) = (a,b,c)
