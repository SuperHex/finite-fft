{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module FiniteField  where

import GHC.TypeLits
import Data.Proxy
import Data.Bits
import Data.Maybe (fromMaybe)

newtype Poly = Poly { getPoly :: Integer }
  deriving (Num, Eq, Ord, Real, Enum, Integral, Show)

class IsInt n where
  fromInt :: Integer -> n
  toInt   :: n -> Integer

class (Num f, Integral f, IsInt f, KnownNat n) =>
      Finite (f :: *) (n :: Nat) | f -> n where
  (.+) :: f -> f -> f
  a .+ b = mod (a + b) (fromInt $ natVal @n Proxy)
  (.*) :: f -> f -> f
  a .* b = mod (a * b) (fromInt $ natVal @n Proxy)
  (.-) :: f -> f -> f
  a .- b = mod (a - b) (fromInt $ natVal @n Proxy)
  (.^) :: f -> f -> f
  a .^ b | b < 0 = fromMaybe (error "number doesn't have inverse") (inverse (abs b))
         | otherwise = let m = (fromInt $ natVal @n Proxy)
                           power = fmap (fromInt . fromIntegral) . decompose . toInt $ b
                        in product (fmap go power) `mod` m
    where go :: f -> f
          go 0 = a
          go 1 = a .* a
          go n | even n = go (n `div` 2) .* go (n `div` 2)
               | otherwise = a ^ (2 ^ n)
  inverse :: f -> Maybe f
  inverse p = let (d, x, _) = extendGCD p (fromInt $ natVal @n Proxy)
              in case d of
                   1 -> Just $ x `mod` fromInt (natVal @n Proxy)
                   _ -> Nothing

instance IsInt Poly where
  fromInt= Poly
  toInt  = getPoly

instance Finite Poly 41

-- TODO: implement faster binary GCD algorithm described here:
-- https://www.di-mgt.com.au/euclidean.html
extendGCD :: (Integral a) => a -> a -> (a, a, a)
extendGCD n1 n2 = go n1 n2 0 1 1 0
 where
  go a b x1 x2 y1 y2
    | b == 0
    = (a, x2, y2)
    | otherwise
    = let q = a `div` b
          r = a `mod` b
          x = x2 - q * x1
          y = y2 - q * y1
      in  go b r x x1 y y1

decompose :: Integer -> [Int]
decompose n | n <= 0    = []
            | otherwise = go n 0
 where
  go x i | x == 0      = []
         | testBit x 0 = i : go (x `shiftR` 1) (i + 1)
         | otherwise   = go (x `shiftR` 1) (i + 1)
