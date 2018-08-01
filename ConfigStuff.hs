
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
import Types

------ READ CONFIG SCHTUFF ------

diffNameToInt :: String -> Maybe Int
diffNameToInt "Beginner" = Just 0
diffNameToInt "Advanced" = Just 1
diffNameToInt "Expert" = Just 2
diffNameToInt "BeginnerExtra" = Just 3
diffNameToInt "AdvancedExtra" = Just 4
diffNameToInt "ExpertExtra" = Just 5
diffNameToInt "Master" = Just 6
diffNameToInt "MasterExtra" = Just 7
-- diffNameToInt "Unused" = Just 9
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
      comment
      return $ Just $ Entry { getStgID = stgID , getTimeAlotted = timeAlotted , getUnlockData = Unresolved , getGoalList = goalList , isLastStage = False } -- isLastStage will be set in fixEntries
    parseNoTime = return Nothing
    parseYesTime = do
      ws1
      time <- parseNum
      return $ Just time
    parseNoGoalData = return []
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
      return goalEntries

        
        

data NormalLine = DiffLine (Int,String) | BeginEntryListLine String | JumpDistanceSlotsLine (Op,Word16) | EntryTypeLine EntryType | Comment



parseNormalLine :: Parse String NormalLine
parseNormalLine = (comment >> return Comment) <|> beginEntryListLine <|> diffLine <|> jumpDistanceSlotsLine <|> entryTypeLine
  where 
    beginEntryListLine = do
      ws
      tokens "#beginEntryList"
      ws1
      name <- parseName
      comment
      return $ BeginEntryListLine name
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
          return $ DiffLine (slotID,listEntryName)
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
      return $ JumpDistanceSlotsLine (op,num)
    entryTypeLine = do
      ws
      tokens "#entryType"
      ws1
      entryTypeStr <- parseName
      comment
      case entryTypeStr of
        "Vanilla" -> return $ EntryTypeLine Vanilla
        "vanilla" -> return $ EntryTypeLine Vanilla
        "BareBone" -> return $ EntryTypeLine BareBone
        "Barebone" -> return $ EntryTypeLine BareBone
        "bareBone" -> return $ EntryTypeLine BareBone
        "barebone" -> return $ EntryTypeLine BareBone
        "BareBones" -> return $ EntryTypeLine BareBone
        "Barebones" -> return $ EntryTypeLine BareBone
        "bareBones" -> return $ EntryTypeLine BareBone
        "barebones" -> return $ EntryTypeLine BareBone
        _ -> empty




      
  

-- This returns a list of pairs of a entry list with the list of associated difficulty slots, and a jump distance slot
readConfig :: String -> IO ([([Int],EntryList)], Options)
readConfig cfgFileName = do
  cfgFile <- openFile cfgFileName ReadMode
  allLns <- fmap lines $ hGetContents cfgFile
  rt <- parseConfig allLns 
  hClose cfgFile
  return rt

data ConfigLoopRecord = CLR { getLineNum :: Int , 
                              getLines :: [String] , 
                              getEntryLists :: [(String,EntryList)] , 
                              getDiffMap :: [(Int,String)] ,
                              getTmpEntryList :: EntryList ,
                              getEntryListNameMaybe :: Maybe String ,
                              getEntryType :: EntryType ,
                              getJumpDistanceSlotsMaybe :: Maybe (Op,Word16)
                            }

-- 
parseConfig :: [String] -> IO ([([Int],EntryList)], Options)
parseConfig allLns =
  let 
    initRecord = CLR { getLineNum = 1 ,
                          getLines = allLns ,
                          getEntryLists = [] ,
                          getDiffMap = [] ,
                          getTmpEntryList = [] ,
                          getEntryListNameMaybe = Nothing ,
                          getEntryType = Vanilla ,
                          getJumpDistanceSlotsMaybe = Nothing
                        }
  in 
    flip fix initRecord $ 
      \loop curRecord -> do
        let err = die generalErrMsg 
            derr msg = die $ generalErrMsg ++ ": " ++ msg
            generalErrMsg = "Error on line " ++ (show curLineNum)
            curLineNum = getLineNum curRecord
            curLns = getLines curRecord
            curEntryLists = getEntryLists curRecord
            curDiffMap = getDiffMap curRecord
            tmpEntryList = getTmpEntryList curRecord
            curEntryListNameMaybe = getEntryListNameMaybe curRecord
            theEntryType = getEntryType curRecord
            curJumpDistanceSlotsMaybe = getJumpDistanceSlotsMaybe curRecord
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
            -- Barebone and similar specific checks
            when (theEntryType == BareBone) $ do 
              -- For certain entry types, we can't have two difficulties with the same entry list id
              when (length curDiffMap /= (length $ noDups $ map snd curDiffMap)) $
                die "When using barebone entries or similar, two difficulties can\'t point to the same entry list"
              -- Every difficulty must be defined
              when (length curDiffMap /= 8) $ 
                die "When using barebone entries or similar, you must specify all eight difficulties"
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
            return $ (pairs,Opts theEntryType curJumpDistanceSlotsMaybe)

          (ln:lns) -> do
            let contRecord = curRecord { getLineNum = curLineNum + 1 , getLines = lns }
                cont = loop contRecord
            case curEntryListNameMaybe of
              Nothing -> do -- We are not reading an entry list
                case parse parseNormalLine ln of
                  Left _ -> err -- Bad parse
                  Right Comment -> cont -- A comment
                  Right (BeginEntryListLine name) -> -- We saw #beginEntryList
                    loop $ contRecord { getTmpEntryList = [] , 
                                        getEntryListNameMaybe = Just name 
                                      } -- Clear tmp list, update entry list name
                  Right (DiffLine diff) -> -- We saw #diff
                    loop $ contRecord { getDiffMap = diff:curDiffMap } -- Add diff declaration
                  Right (JumpDistanceSlotsLine jd) -> do -- We saw #jumpDistanceSlotsLine
                    when (theEntryType /= Vanilla) $ 
                      derr $ "You can only specify jump distance slots for vanilla entries"
                    loop $ contRecord { getJumpDistanceSlotsMaybe = Just jd } -- Add jump distance slots declaration
                  Right (EntryTypeLine entryType) -> do
                    let errMsg = "If you have an entry type line, it must be the first non-whitespace line in the program"
                    when (not $ null curDiffMap && null curEntryLists && null tmpEntryList) $
                      derr errMsg
                    loop $ contRecord { getEntryType = entryType }
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
                    loop $ contRecord { getEntryLists = (curEntryListName,fixedEntries):curEntryLists , 
                                        getTmpEntryList = [] ,
                                        getEntryListNameMaybe = Nothing
                                      } -- Add entry list, reset tmp list and name
                  Right (Just entry) -> do -- We saw an entry
                    -- If we are using barebone entries, make sure that jump distances must be normal
                    when (theEntryType == BareBone) $ 
                      forM_ (getGoalList entry) $ \(goalType,jd) ->
                        when (jd /= (goalTypeToID goalType + 1)) $
                          derr $ "When using barebone entries, you can\'t specify special jump distances for the goals"
                    loop $ contRecord { getTmpEntryList = entry:tmpEntryList } -- Add entry to tmp list


------ TEST CONFIGS ------
config1 = ["#beginEntryList be","201","202 1800","203","#endEntryList","#beginEntryList un","#endEntryList","#beginEntryList ad","221","222","223 1800","#endEntryList","","#diff Beginner be","#diff Unused be","#diff Advanced ad"]
