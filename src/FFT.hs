{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FFT where

import Data.Bits
import FiniteField
import GHC.TypeLits
import Data.Proxy

padding :: a -> [a] -> [a]
padding x xs = if len .&. (len - 1) == 0
  then xs
  else
    let pow = ceiling . logBase 2 . fromIntegral $ len
    in  xs ++ replicate (2 ^ pow - len) x
  where len = length xs

evenList :: [a] -> [a]
evenList (x : _ : xs) = x : evenList xs
evenList (x     : []) = [x]
evenList []           = []

oddList :: [a] -> [a]
oddList (_ : x : xs) = x : oddList xs
oddList (x     : []) = [x]
oddList []           = []

fft :: forall n f . (KnownNat n, Finite f n) => f -> [f] -> [f]
fft root p = if length p == 1
  then p
  else
    let
      b   = fft (root .^ 2) (evenList p)
      c   = fft (root .^ 2) (oddList p)
      out = zipWith (.+) b $ zipWith
        (.*)
        c
        [ root .^ fromInt (fromIntegral n) | n <- [0 .. length p `div` 2 - 1] ]
      out' = zipWith (.+) b $ zipWith
        (.*)
        c
        [ root .^ fromInt (fromIntegral n)
        | n <- [length p `div` 2 .. length p - 1]
        ]
    in
      out ++ out'

ifft :: forall n f . (KnownNat n, Finite f n) => f -> [f] -> [f]
ifft root l =
  let len = fromIntegral $ length l
      matrix =
        [ [ root .^ (-i * j) | i <- [0 .. len - 1] ] | j <- [0 .. len - 1] ]
  in  fmap
        ( (`mod` fromInt (natVal @n Proxy))
        . (`div` len)
        . foldl1 (.+)
        . flip (zipWith (.*)) l
        )
        matrix

-- actually no need to rearrange
rearrange :: [a] -> [a]
rearrange l = (\idx -> l !! idx) <$> go [0] (length l) 2
 where
  go x n k | k < n = let res = x ++ fmap (+ (n `div` k)) x in go res n (k * 2)
           | otherwise = x ++ fmap (+ 1) x

