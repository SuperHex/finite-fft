{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module FiniteField  where

import           Control.Applicative
import           Data.Bits
import qualified Data.IntMap.Strict  as M
import           Data.Maybe          (fromJust)
import           Data.Proxy
import           GHC.TypeLits
import Data.Coerce (coerce)

newtype Field a = Field { getField :: a }
  deriving (Eq, Ord)

instance (Show a) => Show (Field a) where
  show = show . coerce @(Field a) @a

instance Functor Field where
  {-# INLINE fmap #-}
  fmap = coerce

instance Applicative Field where
  {-# INLINE pure #-}
  pure = coerce
  {-# INLINE liftA2 #-}
  liftA2 = coerce

class (KnownNat n, Applicative f) => Finite (f :: * -> *) (n :: Nat) | f -> n where
  (.%) :: (Integral a) => f a -> a -> f a
  a .% b = (`mod` b) <$> a
  (.+) :: (Num a, Integral a) => f a -> f a -> f a
  a .+ b = liftA2 (+) a b .% fromIntegral (natVal @n Proxy)
  (.*) :: (Num a, Integral a) => f a -> f a -> f a
  a .* b = liftA2 (*) a b .% fromIntegral (natVal @n Proxy)
  (.-) :: (Num a, Integral a) => f a -> f a -> f a
  a .- b = liftA2 (-) a b .% fromIntegral (natVal @n Proxy)
  (.^) :: (Num a, Integral a) => f a -> Int -> f a
  a .^ b | b < 0 = (inverse $ a .^ abs b)
         | otherwise = let power = decompose (fromIntegral b)
                        in go (M.insert 0 a M.empty) power
    where go :: (Num a, Integral a) => M.IntMap (f a) -> [Int] -> f a
          go _ [] = pure 1
          go m (x : xs) = case M.lookup x m of
            Nothing -> case x of
              0 -> fromJust (M.lookup 0 m) .* go m xs
              _ -> let subIdx = x - 1
                       sub = go m [subIdx]
                       res = sub .* sub
                       newMap = M.insert subIdx sub . M.insert x res $ m
                        in res .* go newMap xs
            (Just n) -> n .* go m xs
  inverse :: (Num a, Integral a) => f a -> f a
  inverse p = let gcd = liftA2 extendGCD p (pure . fromIntegral $ natVal @n Proxy)
              in fmap (\(d, x, _) -> case d of
                          1 -> x
                          _ -> error "no inverse exist") gcd



-- TODO: implement faster binary GCD algorithm described here:
-- https://www.di-mgt.com.au/euclidean.html
{-# SPECIALIZE extendGCD :: Int -> Int -> (Int, Int, Int) #-}
{-# SPECIALIZE extendGCD :: Integer -> Integer -> (Integer, Integer, Integer) #-}
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

{-# SPECIALIZE decompose :: Integer -> [Int] #-}
decompose :: (Integral a) => Integer -> [a]
decompose n | n <= 0    = []
            | otherwise = go n 0
 where
  go x i | x == 0      = []
         | testBit x 0 = i : go (x `shiftR` 1) (i + 1)
         | otherwise   = go (x `shiftR` 1) (i + 1)

