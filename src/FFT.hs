{-# LANGUAGE DataKinds, FlexibleContexts #-}

module FFT where

import Control.Applicative
import Data.Bits
import FiniteField

normalize :: a -> [a] -> [a]
normalize x xs = if len .&. (len - 1) == 0
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

fftHelper :: (Finite f 41) => [f] -> f -> [f]
fftHelper p root = if length p == 1
  then p
  else
    let
      b   = fftHelper (evenList p) (root ^ 2)
      c   = fftHelper (oddList p) (root ^ 2)
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

ifft :: (Finite f 41) => [f] -> f -> [f]
ifft l root =
  let len = fromIntegral $ length l
      matrix =
        [ [ root .^ (-i * j) | i <- [0 .. len - 1] ] | j <- [0 .. len - 1] ]
  in  fmap (\row -> foldl1 (.+) $ zipWith (.*) row l) matrix

foo l root s =
  let len = fromIntegral $ length l
      matrix =
        [ [ root ** (i * j) `mod` s | i <- [0 .. len - 1] ]
        | j <- [0 .. len - 1]
        ]
  in  fmap (\row -> (`mod` s) . sum $ zipWith (*) row l) matrix


rearrange :: [a] -> [a]
rearrange l = (\idx -> l !! idx) <$> go [0] (length l) 2
 where
  go x n k | k < n = let res = x ++ fmap (+ (n `div` k)) x in go res n (k * 2)
           | otherwise = x ++ fmap (+ 1) x

fft :: (Finite f 41) => [f] -> f -> [f]
fft p root = rearrange (fftHelper p root)
