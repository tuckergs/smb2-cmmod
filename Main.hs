
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

import CodeStuff
import ConfigStuff
import EntryStuff

main = do
  args <- getArgs
  when (length args /= 3) $
    die "Usage: ./Main [input REL] [level order config] [output REL]"
  let (inFileName:configFileName:outFileName:_) = args

  -- Read config and set up things
  (cfgPairs,jumpDistanceSlotsMaybe) <- readConfig configFileName
  let (offs,sz) = computeOffsets cfgPairs
      allEntries = map snd cfgPairs
  hPutStrLn stderr $ "Bytes needed to output entries: 0x" ++ (show $ Hex sz)
  when (sz > maxSize) $ do
    hPutStrLn stderr "The entries won't fit! You need to use under 0x3E3C bytes!"
    exitFailure

  -- Write stuff now
  inFile <- openFile inFileName ReadMode
  outFile <- openFile outFileName WriteMode
  hSetBinaryMode inFile True
  hSetBinaryMode outFile True
  hSetBuffering outFile (BlockBuffering Nothing)
  let cpByte = hGetChar inFile >>= hPutChar outFile
      cpBytes ls = (mapM_ (const cpByte) ls) >> hFlush outFile
  -- Copy up to jump distance code
  hPutStrLn stderr "Copying bytes up to jump distance code..."
  cpBytes [1..startOfJumpDistanceCode]
  -- Write jump distance code
  case jumpDistanceSlotsMaybe of
    Nothing -> do
      hPutStrLn stderr "No jump distance slots specified. Defaulting to == 0"
      writeJumpDistanceCode inFile outFile (Equal,0)
      -- cpBytes [1..8]
    Just jumpDistanceSlots -> do
      hPutStrLn stderr "Writing jump distance code..."
      writeJumpDistanceCode inFile outFile jumpDistanceSlots
  -- Copy up to challenge mode entries
  hPutStrLn stderr "Copying up to challenge mode entry area..."
  cpBytes [startOfJumpDistanceCode+8..startOfCMArea-1]
  -- Write challenge mode entries
  hPutStrLn stderr "Writing challenge mode entries..."
  writeAllEntries outFile allEntries
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
  writeOffsetTable inFile outFile offs
  -- Copy the rest
  hPutStrLn stderr "Copying the rest of file..."
  hSeek inFile AbsoluteSeek afterOffsetTable
  cpBytes [afterOffsetTable..sizeOfRel-1]
  hFlush outFile
  putStrLn "Finished!"
  hClose inFile
  hClose outFile
  
  
