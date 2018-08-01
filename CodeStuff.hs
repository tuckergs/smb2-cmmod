
{-# LANGUAGE LambdaCase #-}

module CodeStuff where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Monoid
import Data.Char
import Data.Word
import System.Environment
import System.Exit
import System.IO

import Helpers
import HexStuff
import ParseMonad
import Types


startOfJumpDistanceCode :: Integer
startOfJumpDistanceCode = 0xa36d8


modCmp :: Word32 -> Word16 -> Word32
modCmp inst imm = (inst .&. 0xFFFF0000) .|. (fromIntegral $ imm)




modBc :: Word32 -> Op -> Word32
modBc inst op = (inst .&. 0xFC00FFFF) .|. (opToBits op)


writeJumpDistanceCode :: Handle -> Handle -> (Op,Word16) -> IO ()
writeJumpDistanceCode inFile outFile (op,num) = do
  BB.hPutBuilder outFile $ BB.word32BE $ modCmp 0x2C000000 num
  BB.hPutBuilder outFile $ BB.word32BE $ modBc 0x40000014 $ opposite op
  hSeek inFile RelativeSeek 8
