{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module TgaLoader where

import Data.Word
import Data.Binary
import Foreign
import qualified Data.ByteString as B
import System.IO
import Data.ByteString.Internal

uncompressedHeader ::  [Word8]
uncompressedHeader = [0,0,2,0,0,0,0,0,0,0,0,0]

compressedHeader :: [Word8]
compressedHeader = [0,0,10,0,0,0,0,0,0,0,0,0] 

data TGA = TGA { bpp :: Int, w ::  Int, h :: Int } deriving Show

instance Binary TGA where
  put = undefined
  get = loadTga

loadTga :: Get TGA
loadTga = do
  header <- mapM (\_ -> get) [(0::Int)..11] :: Get [Word8]
  loadTga' header  

loadTga' :: [Word8] -> Get TGA
loadTga' header | header == uncompressedHeader = loadUncompressed
                | header == compressedHeader = error "compressed not implemented"
                | otherwise = error "unknown file format"

el :: [Word8] -> Int -> Int
el xs n = fromIntegral $ xs!!n

loadUncompressed :: Get TGA
loadUncompressed = do
  header <- mapM (\_ -> get) [(0::Int)..5] :: Get [Word8]
  let width  = (el header 1) * 256 + el header 0
  let height = (el header 3) * 256 + el header 2
  let bpp' :: Int = el header 4
  --let infoStr = printf "width: %d, height: %d, bpp: %d" width height bpp
  return $ TGA { bpp = bpp', w = width, h = height}

loadTgaPtr :: FilePath -> (Maybe TGA -> Ptr Word8 -> IO b) -> IO b
loadTgaPtr path fun = do
  descr :: TGA <- decodeFile path
  let size = bpp descr * (w descr) * (h descr)
  hdl <- openFile path ReadMode
  hSeek hdl AbsoluteSeek 18
  bytestring <- B.hGet hdl size
  let  (fptr,_,_) = toForeignPtr bytestring
  r <- withForeignPtr fptr (fun $ Just descr)
  hClose hdl
  return r
 
test :: IO TGA
test = decodeFile "../data/sponza/textures/sponza_fabric_diff.tga" 
