{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FFT where

import Control.Parallel
import Data.Bits
import FiniteField
import GHC.TypeLits
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Control.Monad.ST
import Control.Monad
import Data.STRef

padding :: a -> [a] -> [a]
padding x xs = if len .&. (len - 1) == 0
  then xs
  else
    let pow = ceiling . logBase 2 . fromIntegral $ len
    in  xs ++ replicate (2 ^ pow - len) x
  where len = length xs

-- evenList :: [a] -> [a]
-- evenList (x : _ : xs) = x : evenList xs
-- evenList (x     : []) = [x]
-- evenList []           = []

-- oddList :: [a] -> [a]
-- oddList (_ : x : xs) = x : oddList xs
-- oddList (x     : []) = [x]
-- oddList []           = []

{-# SPECIALIZE INLINE
    oddEven :: MV.STVector s Int -> ST s (MV.STVector s Int, MV.STVector s Int)
  #-}
oddEven :: (MV.STVector s a) -> ST s (MV.STVector s a, MV.STVector s a)
oddEven v = case MV.length v of
  n -> do
    let half = n `div` 2
    od <- MV.new half
    ev <- MV.new half
    forM_ [0, 2 .. n - 1] $ \i -> do
      let idx = i `div` 2
      MV.unsafeWrite ev idx =<< MV.unsafeRead v i
      MV.unsafeWrite od idx =<< MV.unsafeRead v (i + 1)
    return (od, ev)
  -- assume the input vector's length is power of 2

fft
  :: forall n f a
   . (KnownNat n, Finite f n, Integral a)
  => f a
  -> [f a]
  -> V.Vector (f a)
fft root p = runST $ do
  mv <- V.thaw (V.fromList p)
  v' <- worker root mv
  V.freeze v'
 where
  worker :: f a -> MV.STVector s (f a) -> ST s (MV.STVector s (f a))
  worker r v = case MV.length v of
    0 -> return v
    1 -> return v
    2 -> do
      a0 <- MV.unsafeRead v 0
      a1 <- MV.unsafeRead v 1
      MV.unsafeWrite v 0 (a0 .+ a1) >> MV.unsafeWrite v 1 (a0 .- a1)
      return v
    n -> do
      (od, ev) <- oddEven v
      b        <- worker (r .^ 2) ev
      c        <- worker (r .^ 2) od
      let k = n `div` 2
      m <- newSTRef (r .^ 0)
      forM_ [0, 1 .. k - 1] $ \ix -> do
        bi <- MV.unsafeRead b ix
        ci <- MV.unsafeRead c ix
        m' <- readSTRef m
        MV.unsafeWrite v ix       (bi .+ (m' .* ci))
        MV.unsafeWrite v (ix + k) (bi .- (m' .* ci))
        modifySTRef' m (.* r)
      return v
        -- let out = V.zipWith (.+) b
        --       $ V.zipWith
        --       (.*)
        --       c
        --       [ root .^ fromIntegral n | n <- [0 .. length p `div` 2 - 1] ]
        --     out' = zipWith (.+) b $ zipWith
        --       (.*)
        --       c
        --       [ root .^ fromIntegral n | n <- [length p `div` 2 .. length p - 1] ]
        -- b `par` c `pseq` out ++ out'

ifft
  :: forall n f a
   . (KnownNat n, Finite f n, Integral a)
  => f a
  -> [f a]
  -> [f a]
ifft root l =
  let len = length l
      matrix =
        [ [ root .^ (-i * j) | i <- [0 .. len - 1] ] | j <- [0 .. len - 1] ]
  in  fmap
        ( (.* (inverse . pure . fromIntegral) len)
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

