
{-# LANGUAGE LambdaCase #-}

module EntryStuff where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Monoid
import Data.Char
import Data.List
import Data.Word
import System.Environment
import System.Exit
import System.IO

import Helpers
import HexStuff
import ParseMonad
import Types


------ IMPORTANT CONSTANTS ------

-- The number we add to the offsets for the pointer table at 0x2C7874
magicNumber :: Word16
magicNumber = 0x3550

-- The maximum size the challenge mode entries can be
maxSize :: Word16
maxSize = 0x3C7C

-- The start of the space alloted for the challenge mode entries
startOfCMArea :: Integer
startOfCMArea = 0x2075B0

-- The end of the challenge mode entry space. This is where the RAM pointer table starts
endOfCMArea :: Integer
endOfCMArea = 0x20B22C

-- The first offset of the offset table
firstOffset :: Integer
firstOffset = 0x2C787A

-- Where we leave off after writing the offset table
afterRelevantOffsets :: Integer
afterRelevantOffsets = 0x2C78C2

sizeOfRel :: Integer
sizeOfRel = 3000268

------ ENTRY SCHTUFF ------

twelveBlock = BB.lazyByteString $ BL.pack $ take 48 $ repeat 0x00

sixBlock = BB.lazyByteString $ BL.pack $ take 24 $ repeat 0x00

fiveBlock = BB.lazyByteString $ BL.pack $ take 20 $ repeat 0x00


writeEntry :: Handle -> EntryType -> Entry -> IO ()
writeEntry handle Vanilla EndOfEntries = BB.hPutBuilder handle $ BB.word32BE 0x03000000 <> sixBlock
writeEntry handle Vanilla (Entry stgID timeAlotted _ goalList isEnd) = BB.hPutBuilder handle $
  BB.word32BE 0x02000000
  <> BB.word16BE 0x0000 <> BB.word16BE stgID
  <> timeBuilder
  <> goalListBuilder
  <> fiveBlock
    where 
      timeBuilder = case timeAlotted of
        Nothing -> BB.lazyByteString BL.empty
        Just tm ->
          fiveBlock
          <> BB.word32BE 0x02010000
          <> BB.word16BE 0x0000 <> BB.word16BE tm
      singleGoalBuilder (ty,dist) =
          twelveBlock
        <> BB.word32BE 0x00020000
        <> BB.word32BE (goalTypeToID ty)
        <> fiveBlock
        <> BB.word32BE 0x01000000
        <> BB.word32BE dist
      goalListBuilder =
        if isEnd
          then
            twelveBlock
            <> BB.word32BE 0x01020000
            <> BB.word32BE 0x00000000
          else
            case goalList of
              [] ->
                twelveBlock
                <> BB.word32BE 0x01000000
                <> BB.word32BE 0x00000001
              _ -> 
                mconcat $ map singleGoalBuilder goalList
writeEntry handle BareBone EndOfEntries = BB.hPutBuilder handle $
  BB.word32BE 0x03000000 <> BB.word32BE 0x0
writeEntry handle BareBone (Entry stgID timeAllotted unlockData goalList _) = BB.hPutBuilder handle $
  BB.word8 0x02
  <> (BB.word8 $ computeGoalBitField goalList)
  <> BB.word16BE stgID
  <> timePart
  <> unlockPart
  where
    timePart = BB.word16BE $ case timeAllotted of
      Just tm -> tm
      Nothing -> 3600
    unlockPart = case unlockData of
      Counter loc val -> BB.word8 loc <> BB.word8 val
      BitField bit -> BB.word8 0xff <> BB.word8 bit

writeAllEntries :: Handle -> EntryType -> [EntryList] -> IO ()
writeAllEntries handle entryType diffs = (mapM_ (writeEntry handle entryType) $ concat diffs) >> hFlush handle


------ COMPUTATIONZ! ------

-- Finds what goals exist for the goal list
computeGoalBitField :: [GoalEntry] -> Word8
computeGoalBitField = foldl (\pr -> (.|. pr) . shiftL 1 . goalTypeToID) 1 . map fst


-- Calculates the unlock data for the entries
computeUnlockData :: UnlockScheme -> [(Int,EntryList)] -> IO [(Int,EntryList)]
computeUnlockData Optimized diffs = do
  let
    beDiffSet = [0,3]
    adDiffSet = [1,4]
    exDiffSet = [2,5,6,7]
    diffSets = beDiffSet:adDiffSet:exDiffSet:[]
    -- (numCounters,counterLocations) = fmap reverse $ brutalUncons $ tail $ foldl (\locs@(prevLoc,restLoc) n -> (prevLoc+n):locs) [0] $ counterDataForDiffList diffList
    -- counterDataForDiffList = map $ (`div` 255) . fromIntegral . sum . map (length . flip brutalLookup diffs)
    -- Pads entries with either True or False based on if it is skippable or not
    entriesWithSkippability strippedEntryList = 
      let
        entriesWithLevelNums = zip [1..] strippedEntryList
        skippableForEntry (levelNum,entry) = foldl union [] $ flip map (getGoalList entry) $ \(_,jd) -> take (fromIntegral jd-1) [(levelNum+1)..]
        skippableLevelNums = foldl union [] $ map skippableForEntry entriesWithLevelNums
        unskippableLevelNums = [1..(length strippedEntryList)] \\ skippableLevelNums
        skippableEntryListWithNums = zip skippableLevelNums $ map (\n -> (brutalLookup n entriesWithLevelNums, True)) skippableLevelNums
        unskippableEntryListWithNums = zip unskippableLevelNums $ map (\n -> (brutalLookup n entriesWithLevelNums, False)) unskippableLevelNums
        pairSorter (n1,_) (n2,_) = compare n1 n2
      in map snd $ sortBy pairSorter $ skippableEntryListWithNums ++ unskippableEntryListWithNums 
    numSkippables = sum $ map (length . filter (\(_,isSkippable) -> isSkippable)) $ map (entriesWithSkippability . init) $ map snd diffs
    numUnskippablesGivenDiffId diffId = length $ filter (\(_,isSkippable) -> not isSkippable) $ entriesWithSkippability $ init $ brutalLookup diffId diffs
    numUnskippablesGivenDiffSet = sum . map numUnskippablesGivenDiffId
    numCountersForAllDiffSets = map ((+1) . (`div` 255) . (+ (-1)) . numUnskippablesGivenDiffSet) diffSets
    (numCounters,counterOffsets) = fmap reverse $ brutalUncons $ foldl (\offs@(prevOff:_) n -> (prevOff+n):offs) [0] $ numCountersForAllDiffSets

  hPutStrLn stderr $ "Num counters needed: " ++ (show numCounters)
  hPutStrLn stderr $ "Number of skippable levels: " ++ (show numSkippables)
  when ((8*numCounters + numSkippables) > 160) $
    die $ "You have either too many skippable levels (you can skip these in Challenge Mode) or just too many entries"
  let 
    initCounterData = take 3 $ repeat (0 :: Int)
    counterDataToUse curCounterData diffId = 
      let 
        curIndex = brutalLookup diffId $ map (flip (,) 0) beDiffSet ++ map (flip (,) 1) adDiffSet ++ map (flip (,) 2) exDiffSet
        curNumber = curCounterData !! curIndex
        offset = counterOffsets !! curIndex
        nextNumber = curNumber + 1
        (whatLoc,whatValue) = curNumber `divMod` 255
        nextCounterData = take curIndex curCounterData ++ [nextNumber] ++ drop (curIndex+1) curCounterData
      in ((fromIntegral (whatLoc + offset),fromIntegral $ whatValue+1),nextCounterData)
    addUnlockDataToEntryList (stepCounterData,stepBit) (diffId,entryList) = 
      flip fix (entriesWithSkippability $ init entryList,[],stepCounterData,stepBit) $ \loop (curEntryListWithSkips,madeEntryList,curCounterData,curBit) ->
        case curEntryListWithSkips of
          [] -> ((diffId,reverse $ EndOfEntries:madeEntryList),(curCounterData,curBit))
          ((entry,isSkippable):rest) -> 
            if isSkippable
              then 
                let
                  nextBit = curBit + 1
                  fixedEntry = entry { getUnlockData = BitField curBit }
                in loop $ (rest,fixedEntry:madeEntryList,curCounterData,nextBit)
              else
                let
                  ((curLoc,curData),nextCounterData) = counterDataToUse curCounterData diffId
                  fixedEntry = entry { getUnlockData = Counter curLoc curData }
                in loop $ (rest,fixedEntry:madeEntryList,nextCounterData,curBit) 
    addUnlockDataToAllEntryLists = flip fix (diffs,(initCounterData,fromIntegral $ 8*numCounters)) $ \loop (curDiffs,curCounterBitPair) ->
      case curDiffs of
        [] -> []
        (diff:rest) ->
          let (fixedDiff,nextCounterBitPair) = addUnlockDataToEntryList curCounterBitPair diff
          in (fixedDiff:) $ loop (rest,nextCounterBitPair) 
  return $ addUnlockDataToAllEntryLists

computeUnlockData Stable diffs = do
  let
    pairSorter (d1,_) (d2,_) = compare d1 d2
    sortedDiffs = sortBy pairSorter diffs
    vanillaNumLevels = [10,30,50,10,10,10,10,10]
    vanillaFirstBits = [0,10,40,90,100,110,120,130]
    diffNames = [
      "Beginner","Advanced","Expert",
      "Beginner Extra","Advanced Extra","Expert Extra",
      "Master","Master Extra" ]
    updateListStep (curDiffs,curExtraBitsUsed) (diff@(_,entryList),numLevels,firstBit) =
      let
        entriesBeforeCutoff = take numLevels entryList
        entriesAfterCutoff = drop numLevels entryList
        numLevelsAfterCutoff = fromIntegral $ max 0 $ length entriesAfterCutoff - 1
        updater = zipWith $ \entry b -> case entry of
          EndOfEntries -> EndOfEntries
          _ -> entry { getUnlockData = BitField b }
        updatedBeforeCutoff = updater entriesBeforeCutoff [firstBit..]
        updatedAfterCutoff = updater entriesAfterCutoff [(140+curExtraBitsUsed)..]
        updatedDiff = flip (<$) diff $ updatedBeforeCutoff ++ updatedAfterCutoff
      in (curDiffs ++ [updatedDiff],curExtraBitsUsed + numLevelsAfterCutoff)
    (updatedDiffs,numExtraBitsUsed) = foldl updateListStep ([],0) $ zipWith3 (,,) sortedDiffs vanillaNumLevels vanillaFirstBits
  when (numExtraBitsUsed > 20) $
    die $ "When using barebone entries or similar with stable unlockedness, you can\'t have more than 20 levels beyond the vanilla difficulty bounds (like Beginner 11).\nUse optimized unlockedness for more than 160 levels; read docs/cmentries.txt before you use them" 
  return updatedDiffs  

------ POINTER SCHTUFF ------

-- The size of an challenge mode entry
entrySize :: EntryType -> Entry -> Word16
entrySize Vanilla EndOfEntries = 0x1C
entrySize Vanilla (Entry _ timeAllottedMaybe _ goalList isEnd) = sz
  where 
    numFromTime = case timeAllottedMaybe of
      Nothing -> 0
      Just _ -> 0x1C
    numFromGoals = if ((null goalList) || isEnd)
      then 0x38
      else (0x54*) $ fromIntegral $ length goalList
    sz = 0x08 + numFromTime + numFromGoals + 0x14
entrySize BareBone _ = 0x8


diffSize :: EntryType -> EntryList -> Word16
diffSize entryType diff = sum $ map (entrySize entryType) diff

-- Offsets of the difficulties from 0x2075B0, as well as the size of all the entries
offsetsAndSize :: EntryType -> [EntryList] -> ([Word16],Word16)
offsetsAndSize entryType = loop (0,[0]) 
  where 
    loop (accum,curOffsets) = \case
      [] -> (\(sz:offs) -> (reverse offs,sz)) $ curOffsets
      (d:ds) -> 
        let newOffset = accum + diffSize entryType d
        in loop (newOffset,newOffset:curOffsets) ds


-- Compute offsets given the pairs of slot ids with their entry lists
computeOffsets :: EntryType -> [([Int],EntryList)] -> ([Word16],Word16)
computeOffsets entryType pairs = 
  let 
    (offsets,sz) = offsetsAndSize entryType $ map snd pairs
    offsetPairs = zip (map fst pairs) $ offsets -- Replace entry lists with their size
    offsetList = concat $ flip map offsetPairs $ \(ls,off) -> map (flip (,) off) ls -- Breaks up slot IDs into different pairs
    offsetList2 = (offsetList ++) $ flip zip (repeat 0x0) $ ([1..9] \\) $ map fst offsetList -- Adds other difficulties in unspecified
    cmpPairsBySlotID (a,_) (b,_)
      | a < b = LT
      | a == b = EQ
      | a > b = GT
    sortedOffsetList = sortBy cmpPairsBySlotID offsetList2
  in (map snd sortedOffsetList,sz)

  

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
writeOffsetTable inFile outFile (off1:off2:off3:off4:off5:off6:off7:off8:_) = do
  hSeek inFile RelativeSeek 0x2
  mapM_ (writeOffset inFile outFile) $ (off1:off2:off3:off4:off5:off6:off7:off8:off8:[]) 
  

