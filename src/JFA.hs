{-# LANGUAGE BangPatterns #-}

module JFA where

import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

quad :: Num a => (a, a) -> (a, a) -> a
quad (x,y) (u,v) = (x-u)^2 + (y-v)^2
{-# INLINE quad #-}

linToIx :: Integral a => a -> a -> (a, a)
linToIx w i = i `quotRem` w
{-# INLINE linToIx #-}

ixToLin :: Num a => a -> a -> a -> a
ixToLin w y x = w * y + x
{-# INLINE ixToLin #-}

interleave4 :: [a] -> [a] -> [a] -> [a] -> [a]
interleave4 (a:as) (b:bs) (c:cs) (d:ds) = a:b:c:d:interleave4 as bs cs ds
interleave4 _ _ _ _ = []

jfa :: (Functor f, Foldable f)
    => f (Int, Int)  -- ^ @vs@ foldable container of occupied (x,y) points
    -> Int -- ^ @w@ width
    -> Int -- ^ @w@ height
    -> V.Vector (Int, Int, Int) -- ^ resulting euclidean 'distance' map
                               -- as flat vector of (x,y,q) triples
                               -- where (x,y) is the nearest occupied
                               -- point, and q is the quadrance
                               -- (squared distance) to that point
jfa vs w h = V.map (\(d,i) -> let (x,y) = linToIx w i in (x,y,d))
    $ foldr (\k v -> V.imap (update v k) v) vec (interleave4 steps (fmap (* w) steps) (fmap (* (w-1)) steps) (fmap (* (w+1)) steps))
    where
      vec = V.modify (\v -> do
                           forM_ vs' (\(d,i) -> M.write v i (d,i)))
            $ V.replicate (w*h) (maxBound, maxBound)
      vs' = fmap (\(y,x) -> (0, ixToLin w x y)) vs
      -- takes the vector to update,
      -- the index of the point to update,
      -- the current (quadrance,index) pair stored at that point
      -- produces the new best (quadrance,index) pair
      update !v k pnt (da,ia) = minimum
                               $ (da,ia) :
                               (fmap (\i -> let d = quad' pnt i in if i < maxBound
                                                                     && d < da
                                                                  then (d,i)
                                                                  else (maxBound,i))
                               $ fmap (\i -> snd (v `V.unsafeIndex` i))
                               $ filter (\i -> 0 <= i && i <= (w * h) -1)
                               $ [pnt-k, pnt+k])
      {-# INLINE update #-}
      quad' i1 i2 = quad (linToIx w i1) (linToIx w i2)
      {-# INLINE quad' #-}
      -- descending step size
      steps = let k = (\a -> a - 1) . ceiling $ logBase 2 (fromIntegral (max w h))
              in fmap (\a -> 2^a) ([0..k])
                 -- append an additional 0 for JFA+1, preppend for 1+JFA
