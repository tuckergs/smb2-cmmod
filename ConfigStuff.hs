
{-# LANGUAGE LambdaCase #-}

module ConfigStuff (getEntriesFromConfig) where

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

import EntryStuff (Entry(..),Difficulty)

------ READ CONFIG SCHTUFF ------

parseNum :: Parse String Word16
parseNum = fmap read $ (list1 $ spot $ isDigit) <|> (tokens "0x" >> fmap ("0x"++) (list1 $ spot $ isHexDigit))
  

parseConfigLine :: Parse String (Maybe Entry)
parseConfigLine = (comment >> return Nothing) <|> entryLine <|> endOfEntryLine
  where 
    comment = ws <|> (ws >> token '%' >> list item)
    endOfEntryLine = do
      ws
      token '$'
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


-- Splits up entries by difficulty and flips last level entry bits
fixEntries :: [Entry] -> [[Entry]]
fixEntries allEntries = loop (False,allEntries,[],[]) 
  where 
    loop (prevWasEnd,curEntries,tmpEntryList,curDiffs) =
      case curEntries of
        [] -> filter (\case {[] -> False ; _ -> True}) $ tmpEntryList:curDiffs
        EndOfEntries:es -> loop (True,es,[EndOfEntries],tmpEntryList:curDiffs)
        e:es ->
          if prevWasEnd
            then 
              let fixedE = e { isLastStage = True }
              in loop (False,es,fixedE:tmpEntryList,curDiffs)
            else loop (False,es,e:tmpEntryList,curDiffs)

getEntriesFromConfig :: String -> IO [[Entry]]
getEntriesFromConfig cfgFileName = do
  cfgFile <- openFile cfgFileName ReadMode
  allLns <- fmap lines $ hGetContents cfgFile
  entries <- flip fix (1,allLns,[]) $ 
    \loop (curLineNum,curLns,curEntries) -> do
      case curLns of
        [] -> return $ fixEntries curEntries
        (ln:lns) -> do
          case parse parseConfigLine ln of
            Left _ -> do
              hPutStrLn stderr $ "Parse error on line " ++ (show curLineNum)
              hClose cfgFile
              exitFailure
            Right Nothing -> loop (curLineNum+1,lns,curEntries)
            Right (Just e) -> loop (curLineNum+1,lns,e:curEntries)
  hClose cfgFile
  return entries
