
{-# LANGUAGE LambdaCase #-}

module ConfigStuff (readConfig) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
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

import CodeStuff
import EntryStuff

------ READ CONFIG SCHTUFF ------

diffNameToInt :: String -> Maybe Int
diffNameToInt "Beginner" = Just 1
diffNameToInt "Advanced" = Just 2
diffNameToInt "Expert" = Just 3
diffNameToInt "BeginnerExtra" = Just 4
diffNameToInt "AdvancedExtra" = Just 5
diffNameToInt "ExpertExtra" = Just 6
diffNameToInt "Master" = Just 7
diffNameToInt "MasterExtra" = Just 8
diffNameToInt "Unused" = Just 9
diffNameToInt _ = Nothing

comment = ws <|> (ws >> token '%' >> list item)

parseNum :: Read a => Parse String a
parseNum = fmap read $ (list1 $ spot $ isDigit) <|> (tokens "0x" >> fmap ("0x"++) (list1 $ spot $ isHexDigit))

parseName :: Parse String String
parseName = do
  c <- spot isAlpha
  cs <- list $ spot isAlphaNum
  return (c:cs)
  
parseEntryListLine :: Parse String (Maybe Entry)
parseEntryListLine = (comment >> return Nothing) <|> entryLine <|> endOfEntryLine
  where 
    endOfEntryLine = do
      ws
      tokens "#endEntryList" 
      comment
      return $ Just $ EndOfEntries
    entryLine = do
      ws
      stgID <- parseNum
      timeAlotted <- parseNoTime <|> parseYesTime  
      goalList <- parseNoGoalData <|> parseYesGoalData
      return $ Just $ Entry { getStgID = stgID , getTimeAlotted = timeAlotted , getGoalList = goalList , isLastStage = False } -- isLastStage will be set in fixEntries
    parseNoTime = return Nothing
    parseYesTime = do
      ws1
      time <- parseNum
      return $ Just time
    parseNoGoalData = comment >> return []
    parseYesGoalData = do
      ws1
      token '|'
      goalListMaybes <- list1 $ do
        ws1
        goalName <- parseName
        let store = [("blue",BlueG),("green",GreenG),("red",RedG)]
        case lookup (map toLower goalName) store of
          Nothing -> return Nothing
          Just goalType -> do
            goalDist <- ((return $ (+1) $ goalTypeToID goalType) <|>) $ do
              token '='
              parseNum
            return $ Just (goalType,goalDist)
      let cmpGoalEntries (ty1,_) (ty2,_) = compare ty1 ty2
      goalEntries <- fmap (sortBy cmpGoalEntries) $ flip fix goalListMaybes $ \loop -> \case
        [] -> return []
        (Nothing:_) -> empty
        ((Just g):gMaybes) -> do
          gs <- loop gMaybes
          return (g:gs)
      let 
        hasDuplicates = not $ null $ do
          let goalTypesWithIndices = zip [0..] $ map fst goalEntries
          (i,ty1) <- goalTypesWithIndices
          (j,ty2) <- goalTypesWithIndices
          guard (ty1 == ty2)
          guard (i /= j)
          return "erg"
      when hasDuplicates $ empty
      comment
      return goalEntries

        
        

data NormalLine = DiffLine (Int,String) | BeginEntryListLine String | JumpDistanceSlotsLine (Op,Word16)



parseNormalLine :: Parse String (Maybe NormalLine)
parseNormalLine = (comment >> return Nothing) <|> beginEntryListLine <|> diffLine <|> jumpDistanceSlotsLine
  where 
    beginEntryListLine = do
      ws
      tokens "#beginEntryList"
      ws1
      name <- parseName
      comment
      return $ Just $ BeginEntryListLine name
    diffLine = do
      ws
      tokens "#diff"
      ws1
      diffName <- parseName
      case diffNameToInt diffName of
        Nothing -> empty
        Just slotID -> do
          ws1
          listEntryName <- parseName
          comment
          return $ Just $ DiffLine (slotID,listEntryName)
    jumpDistanceSlotsLine = do
      ws
      tokens "#jumpDistanceSlots"
      ws1
      op <- 
        (tokens "==" >> return Equal)
        <|> (tokens "!=" >> return NotEqual)
        <|> (tokens ">=" >> return GreaterThanEqual)
        <|> (tokens "<=" >> return LessThanEqual)
        <|> (token '>' >> return GreaterThan)
        <|> (token '<' >> return LessThan)
      ws
      num <- parseNum
      comment
      return $ Just $ JumpDistanceSlotsLine (op,num)



      
  

-- This returns the unzip of a list of pairs of a entry list with the list of associated difficulty slots
readConfig :: String -> IO ([([Int],EntryList)], Maybe (Op,Word16))
readConfig cfgFileName = do
  cfgFile <- openFile cfgFileName ReadMode
  allLns <- fmap lines $ hGetContents cfgFile
  rt <- parseConfig allLns 
  hClose cfgFile
  return rt

-- 
parseConfig :: [String] -> IO ([([Int],EntryList)], Maybe (Op,Word16))
parseConfig allLns =
  flip fix (1,allLns,[],[],[],Nothing,Nothing) $ 
    \loop (curLineNum,curLns,curEntryLists,curDiffMap,tmpEntryList,curEntryListNameMaybe,curJumpDistanceSlots) -> do
      let err = die $ "Error on line " ++ (show curLineNum)
      case curLns of
        [] -> do
          let fixedEntryLists = reverse curEntryLists
          -- Check if difficulty declared twice
          when (length curDiffMap /= (length $ noDups $ map fst curDiffMap)) $
            die "A difficulty is declared twice"
          -- Check if difficulty declaration has entry list id that is undefined
          when (length curDiffMap /= length [ 1 | (_,listID) <- curDiffMap , elem listID (map fst fixedEntryLists)]) $ 
            die "A difficulty declaration points to a entry list that doesn\'t exist"
          -- Check if two entry lists have same ID
          when (length fixedEntryLists /= (length $ noDups $ map fst fixedEntryLists)) $ do
            die "Two entry lists have same ID"
          -- Compute pairs 
          let listEntryStrs = noDups $ map snd curDiffMap
              -- compactDiffMap combines pairs with like entry list ids
              compactDiffMap = flip map listEntryStrs $ \s -> flip (,) s $ map fst $ flip filter curDiffMap $ (==s) . snd
              pairs = do
                (listID1,el) <- fixedEntryLists
                (slotIDs,listID2) <- compactDiffMap
                guard $ listID1 == listID2
                return (reverse slotIDs,el)
          -- Return pairs
          return $ (pairs,curJumpDistanceSlots)

        (ln:lns) -> do
          let cont = loop (curLineNum+1,lns,curEntryLists,curDiffMap,tmpEntryList,curEntryListNameMaybe,curJumpDistanceSlots)
          case curEntryListNameMaybe of
            Nothing -> do -- We are not reading an entry list
              case parse parseNormalLine ln of
                Left _ -> err -- Bad parse
                Right Nothing -> cont -- A comment
                Right (Just (BeginEntryListLine name)) -> -- We saw #beginEntryList
                  loop (curLineNum+1,lns,curEntryLists,curDiffMap,[],Just name,curJumpDistanceSlots) -- Clear tmp list, update entry list name
                Right (Just (DiffLine diff)) -> -- We saw #diff
                  loop (curLineNum+1,lns,curEntryLists,diff:curDiffMap,tmpEntryList,curEntryListNameMaybe,curJumpDistanceSlots) -- Add diff declaration
                Right (Just (JumpDistanceSlotsLine jd)) -> -- We saw #jumpDistanceSlotsLine
                  loop (curLineNum+1,lns,curEntryLists,curDiffMap,tmpEntryList,curEntryListNameMaybe,Just jd)
            Just curEntryListName -> do -- We are in entry list
              case parse parseEntryListLine ln of
                Left _ -> err -- Bad parse
                Right Nothing -> cont -- Comment
                Right (Just EndOfEntries) -> do -- We saw #endEntryList
                  let 
                    fixedEntries = reverse $ (EndOfEntries:) $ -- Add EndOfEntries and put the list in normal order
                      case tmpEntryList of
                        [] -> []
                        (e:es) ->
                          let lastE = e { isLastStage = True } -- Last stage bit set
                          in lastE:es 
                  loop (curLineNum+1,lns,(curEntryListName,fixedEntries):curEntryLists,curDiffMap,[],Nothing,curJumpDistanceSlots) -- Add entry list, reset tmp list and name
                Right (Just entry) ->  -- We saw an entry
                  loop (curLineNum+1,lns,curEntryLists,curDiffMap,entry:tmpEntryList,curEntryListNameMaybe,curJumpDistanceSlots) -- Add entry to tmp list

              

------ TEST CONFIGS ------
config1 = ["#beginEntryList be","201","202 1800","203","#endEntryList","#beginEntryList un","#endEntryList","#beginEntryList ad","221","222","223 1800","#endEntryList","","#diff Beginner be","#diff Unused be","#diff Advanced ad"]
