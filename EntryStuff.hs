
{-# LANGUAGE LambdaCase #-}

module EntryStuff (Entry(..),Difficulty,
                    maxSize,magicNumber,
                    startOfCMArea,endOfCMArea,
                    firstOffset,afterOffsetTable,sizeOfRel,
                    writeEntry,writeAllEntries,
                    entrySize,diffSize,
                    offsetsAndSize,writeOffset,writeOffsetTable) where


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


data Entry = EndOfEntries | Entry { getStgID :: Word16 , getTimeAlotted :: Maybe Word16 , isLastStage :: Bool }
  deriving Show

type Difficulty = [Entry]

------ IMPORTANT CONSTANTS ------

-- The number we add to the offsets for the pointer table at 0x2C7874
magicNumber :: Word16
magicNumber = 0x3550

-- The maximum size the challenge mode entries can be
maxSize :: Word16
maxSize = 0x3E3C

-- The start of the space alloted for the challenge mode entries
startOfCMArea :: Integer
startOfCMArea = 0x2075B0

-- The end of the challenge mode entry space. This is where the RAM pointer table starts
endOfCMArea :: Integer
endOfCMArea = 0x20B3EC

-- The first offset of the offset table
firstOffset :: Integer
firstOffset = 0x2C787A

-- Where we leave off after writing the offset table
afterOffsetTable :: Integer
afterOffsetTable = 0x2C78DA

sizeOfRel :: Integer
sizeOfRel = 3000268

------ ENTRY SCHTUFF ------

twelveBlock = BB.lazyByteString $ BL.pack $ take 48 $ repeat 0x00

sixBlock = BB.lazyByteString $ BL.pack $ take 24 $ repeat 0x00

fiveBlock = BB.lazyByteString $ BL.pack $ take 20 $ repeat 0x00


writeEntry :: Handle -> Entry -> IO ()
writeEntry handle EndOfEntries = BB.hPutBuilder handle $ BB.word32BE 0x03000000 <> sixBlock
writeEntry handle (Entry stgID timeAlotted isEnd) = BB.hPutBuilder handle $
  BB.word32BE 0x02000000
  <> BB.word16BE 0x0000 <> BB.word16BE stgID
  <> timeBuilder
  <> twelveBlock
  <> (BB.word32BE $ if isEnd then 0x01020000 else 0x01000000)
  <> (BB.word32BE $ if isEnd then 0x00000000 else 0x00000001)
  <> fiveBlock
    where 
      timeBuilder = case timeAlotted of
        Nothing -> BB.lazyByteString BL.empty
        Just tm ->
          fiveBlock
          <> BB.word32BE 0x02010000
          <> BB.word16BE 0x0000 <> BB.word16BE tm

writeAllEntries :: Handle -> [Difficulty] -> IO ()
writeAllEntries handle diffs = (mapM_ (writeEntry handle) $ concat diffs) >> hFlush handle


------ POINTER SCHTUFF ------

type Offset = Maybe 

-- The size of an challenge mode entry
entrySize :: Entry -> Word16
entrySize EndOfEntries = 0x1C
entrySize (Entry _ Nothing _) = 0x54
entrySize (Entry _ (Just _) _) = 0x70

diffSize :: Difficulty -> Word16
diffSize diff = sum $ map entrySize diff

-- Offsets of the difficulties from 0x2075B0, as well as the size of all the entries
offsetsAndSize :: [Difficulty] -> ([Word16],Word16)
offsetsAndSize = loop (0,[0]) 
  where 
    loop (accum,curOffsets) = \case
      [] -> (\(sz:offs) -> (reverse offs,sz)) $ curOffsets
      (d:ds) -> 
        let newOffset = accum + diffSize d
        in loop (newOffset,newOffset:curOffsets) ds

-- inFile must be at the stuff after the offset
writeOffset :: Handle -> Handle -> Word16 -> IO ()
writeOffset inFile outFile off = do
  BB.hPutBuilder outFile $ BB.word16BE (off+magicNumber)
  hFlush outFile
  forM_ [1..6] $ const $ hGetChar inFile >>= hPutChar outFile
  hSeek inFile RelativeSeek 0x2
  hFlush outFile

-- Note this makes the empty difficulty point to Master Extra
writeOffsetTable :: Handle -> Handle -> [Word16] -> IO ()
writeOffsetTable inFile outFile (off1:off2:off3:off4:off5:off6:off7:off8:off9:_) = do
  hSeek inFile RelativeSeek 0x2
  mapM_ (writeOffset inFile outFile) $ (off1:off2:off3:off4:off5:off6:off7:off8:off8:off9:off9:off9:[]) 
  

