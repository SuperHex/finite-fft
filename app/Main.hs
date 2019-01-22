{-# LANGUAGE TypeApplications, MagicHash, FlexibleInstances, MultiParamTypeClasses, DataKinds #-}

module Main where

import FiniteField
import FFT
import Data.Vector
import Data.Coerce

instance Finite Field 65537

main :: IO ()
main =
  print $ fft (Field @Int 5) (coerce @[Int] @[Field Int] [0 .. 65535]) ! 65535
