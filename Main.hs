
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
import Types

main = do
  args <- getArgs
  when (length args /= 3) $
    die "Usage: ./Main [input REL] [level order config] [output REL]"
  let (inFileName:configFileName:outFileName:_) = args

  -- Read config and set up things
  (cfgPairs,Opts theEntryType theUnlockScheme jumpDistanceSlotsMaybe) <- readConfig configFileName
  let (offs,sz) = computeOffsets theEntryType cfgPairs
  hPutStrLn stderr $ "Bytes needed to output entries: 0x" ++ (show $ Hex sz)
  when (sz > maxSize) $ do
    hPutStrLn stderr "The entries won't fit! You need to use under 0x3C7C bytes!"
    exitFailure

  -- If we are using barebone entries or similar, we have to figure out unlock data for the entries
  pairsWithUnlockData <- 
    if (theEntryType == BareBone)
      then do
        let pairsWithUnlistedIds = map (\(diffIds,entryList) -> (head diffIds,entryList)) cfgPairs
        fmap (map $ \(diffId,entryList) -> ([diffId],entryList)) $ computeUnlockData theUnlockScheme pairsWithUnlistedIds
      else return cfgPairs
  let allEntries = map snd pairsWithUnlockData

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
  -- Write jump distance code or not if we are using vanilla entries
  if (theEntryType == Vanilla)  
    then case jumpDistanceSlotsMaybe of
      Nothing -> do
        hPutStrLn stderr "No jump distance slots specified."
        cpBytes [1..8]
      Just jumpDistanceSlots -> do
        hPutStrLn stderr "Writing jump distance code..."
        writeJumpDistanceCode inFile outFile jumpDistanceSlots
    else cpBytes [1..8]
  -- Copy up to challenge mode entries
  hPutStrLn stderr "Copying up to challenge mode entry area..."
  cpBytes [startOfJumpDistanceCode+8..startOfCMArea-1]
  -- Write challenge mode entries
  hPutStrLn stderr "Writing challenge mode entries..."
  writeAllEntries outFile theEntryType allEntries
  -- Generate random junk
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
  hSeek inFile AbsoluteSeek afterRelevantOffsets
  cpBytes [afterRelevantOffsets..sizeOfRel-1]
  hFlush outFile
  putStrLn "Finished!"
  hClose inFile
  hClose outFile
  
  
