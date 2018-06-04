
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
import Data.Word
import System.Environment
import System.Exit
import System.IO

import Helpers
import HexStuff
import ParseMonad

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

parseNum :: Parse String Word16
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
      return $ Just $ Entry { getStgID = stgID , getTimeAlotted = timeAlotted , isLastStage = False } -- isLastStage will be set in fixEntries
    parseNoTime = do
      comment
      return Nothing
    parseYesTime = do
      ws1
      time <- parseNum
      comment
      return $ Just time


parseNormalLine :: Parse String (Maybe (Either String (Int,String)))
parseNormalLine = (comment >> return Nothing) <|> beginEntryListLine <|> diffLine
  where 
    beginEntryListLine = do
      ws
      tokens "#beginEntryList"
      ws1
      name <- parseName
      comment
      return $ Just $ Left name
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
          return $ Just $ Right (slotID,listEntryName)

      
  

-- This returns the unzip of a list of pairs of a entry list with the list of associated difficulty slots
readConfig :: String -> IO [([Int],EntryList)]
readConfig cfgFileName = do
  cfgFile <- openFile cfgFileName ReadMode
  allLns <- fmap lines $ hGetContents cfgFile
  rt <- parseConfig allLns 
  hClose cfgFile
  return rt

-- 
parseConfig :: [String] -> IO [([Int],EntryList)]
parseConfig allLns =
  flip fix (1,allLns,[],[],[],Nothing) $ 
    \loop (curLineNum,curLns,curEntryLists,curDiffMap,tmpEntryList,curEntryListNameMaybe) -> do
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
          return $ pairs

        (ln:lns) -> do
          let cont = loop (curLineNum+1,lns,curEntryLists,curDiffMap,tmpEntryList,curEntryListNameMaybe)
          case curEntryListNameMaybe of
            Nothing -> do -- We are not reading an entry list
              case parse parseNormalLine ln of
                Left _ -> err -- Bad parse
                Right Nothing -> cont -- A comment
                Right (Just (Left name)) -> -- We saw #beginEntryList
                  loop (curLineNum+1,lns,curEntryLists,curDiffMap,[],Just name) -- Clear tmp list, update entry list name
                Right (Just (Right diff)) -> -- We saw #diff
                  loop (curLineNum+1,lns,curEntryLists,diff:curDiffMap,tmpEntryList,curEntryListNameMaybe) -- Add diff declaration
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
                  loop (curLineNum+1,lns,(curEntryListName,fixedEntries):curEntryLists,curDiffMap,[],Nothing) -- Add entry list, reset tmp list and name
                Right (Just entry) ->  -- We saw an entry
                  loop (curLineNum+1,lns,curEntryLists,curDiffMap,entry:tmpEntryList,curEntryListNameMaybe) -- Add entry to tmp list

              

------ TEST CONFIGS ------
config1 = ["#beginEntryList be","201","202 1800","203","#endEntryList","#beginEntryList un","#endEntryList","#beginEntryList ad","221","222","223 1800","#endEntryList","","#diff Beginner be","#diff Unused be","#diff Advanced ad"]
