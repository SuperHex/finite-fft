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

import           Data.Bits
import qualified Data.IntMap.Strict as M
import           Data.Maybe         (fromJust, fromMaybe)
import           Data.Proxy
import           GHC.TypeLits

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
  a .^ b | b < 0 = fromMaybe (error "number doesn't have inverse") (inverse $ a .^ abs b)
         | otherwise = let power = fmap (fromInt . fromIntegral) . decompose . toInt $ b
                        in go (M.insert 0 a M.empty) power
    where go :: M.IntMap f -> [f] -> f
          go _ [] = fromInt 1
          go m (x : xs) = case M.lookup (fromInteger $ toInt x) m of
            Nothing -> case x of
              0 -> fromJust (M.lookup 0 m) .* go m xs
              _ -> let subIdx = x - 1
                       sub = go m [subIdx]
                       res = sub .* sub
                       newMap = M.insert (fromInteger . toInt $ subIdx) sub . M.insert (fromInteger . toInt $ x) res $ m
                        in res .* go newMap xs
            (Just n) -> n .* go m xs
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

