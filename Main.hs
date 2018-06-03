
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Monoid
import Data.Char
import Data.Word
import System.Environment
import System.Exit
import System.IO

import HexStuff
import ParseMonad

import ConfigStuff
import EntryStuff

main = do
  args <- getArgs
  when (length args /= 3) $
    die "Usage: ./Main [input REL] [level order config] [output REL]"
  let (inFileName:configFileName:outFileName:_) = args

  -- Read config and set up things
  diffs <- getEntriesFromConfig configFileName
  let (offs,sz) = offsetsAndSize diffs
  hPutStrLn stderr $ "Bytes needed to output entries: 0x" ++ (show $ Hex sz)
  when (sz > maxSize) $ do
    hPutStrLn stderr "The entries won't fit! You need to use under 0x3E3C bytes!"
    exitFailure
  when (not $ elem (length offs) [8,9]) $ do
    hPutStrLn stderr "You must have 8 difficulties (with optionally the empty difficulty) in your config file!"
    exitFailure
  let fixedOffs = if (length offs == 8) then (offs ++ [head offs]) else offs

  -- Write stuff now
  inFile <- openFile inFileName ReadMode
  outFile <- openFile outFileName WriteMode
  hSetBinaryMode inFile True
  hSetBinaryMode outFile True
  hSetBuffering outFile (BlockBuffering Nothing)
  let cpByte = hGetChar inFile >>= hPutChar outFile
      cpBytes ls = (mapM_ (const cpByte) ls) >> hFlush outFile
  -- Copy up to challenge mode entries
  hPutStrLn stderr "Copying bytes up to challenge mode entries..."
  cpBytes [1..startOfCMArea]
  -- Write challenge mode entries
  hPutStrLn stderr "Writing challenge mode entries..."
  writeAllEntries outFile diffs
  -- Random junk
  hPutStrLn stderr "Writing dummy data to fill some space..."
  posAfterWritingDiffs <- hTell outFile
  mapM_ (hPutChar outFile) $ take (fromIntegral $ endOfCMArea - posAfterWritingDiffs) $ cycle "BAGEL"
  -- Copy up to offset table
  hPutStrLn stderr "Copying bytes up to first offset in offset table at 0x2C7874..."
  hSeek inFile AbsoluteSeek endOfCMArea
  cpBytes [endOfCMArea..firstOffset-1]
  -- Write offset table
  hPutStrLn stderr "Writing offset table..."
  writeOffsetTable inFile outFile fixedOffs
  -- Copy the rest
  hPutStrLn stderr "Copying the rest of file..."
  hSeek inFile AbsoluteSeek afterOffsetTable
  cpBytes [afterOffsetTable..sizeOfRel-1]
  hFlush outFile
  putStrLn "Finished!"
  hClose inFile
  hClose outFile
  
  
