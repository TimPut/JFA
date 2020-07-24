module Main (main) where

import JFA
import qualified Data.ByteString as BS
import Codec.BMP
import Data.Word
import Data.List
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    let (w,h) = (512,512)
        f n = round $ (255/ fromIntegral w) * (fromIntegral n)
        f' n = round $ (255/ fromIntegral w) * sqrt(fromIntegral n)
        vs = [(511,0),(400,200),(400,511),(511,511)]
        vs' = fmap (\a -> (256 + (round $ 100 * cos a), 256 + (round $ 100 * sin a))) [0,1..2*pi]
    let j = concat
            . fmap (\(x,y,d) -> let d' = f' d in [d', d', d', 0])
            . V.toList $ jfa (vs ++ vs') w h
    let rgba = BS.pack j
    let bmp  = packRGBA32ToBMP24 w h rgba
    writeBMP "voro.bmp" bmp
