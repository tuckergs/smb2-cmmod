
module Types where

import Data.Word

data EntryType = Vanilla | BareBone
  deriving (Show, Eq)

data Options = Opts { optGetEntryType :: EntryType , optGetJumpDistanceSlots :: Maybe (Op,Word16) } 

data GoalType = BlueG | GreenG | RedG
  deriving (Show, Eq, Ord)

goalTypeToID :: Num a => GoalType -> a
goalTypeToID BlueG = 0
goalTypeToID GreenG = 1
goalTypeToID RedG = 2

type GoalEntry = (GoalType,Word32)

data UnlockData = BitField Word8 | Counter Word8 Word8 | Unresolved
  deriving Show

data Entry = EndOfEntries | Entry { getStgID :: Word16 , getTimeAlotted :: Maybe Word16 , getUnlockData :: UnlockData , getGoalList :: [GoalEntry] , isLastStage :: Bool }
  deriving Show

type EntryList = [Entry]

data Op = LessThan | GreaterThan | LessThanEqual | GreaterThanEqual | Equal | NotEqual | AL

opToBits :: Op -> Word32
opToBits GreaterThanEqual = 0x00800000
opToBits LessThanEqual = 0x00810000
opToBits NotEqual  = 0x00820000
opToBits LessThan  = 0x01800000
opToBits GreaterThan  = 0x01810000
opToBits Equal  = 0x01820000
opToBits AL  = 0x02800000

opposite :: Op -> Op
opposite GreaterThanEqual = LessThan
opposite LessThanEqual = GreaterThan
opposite NotEqual = Equal
opposite LessThan = GreaterThanEqual
opposite GreaterThan = LessThanEqual
opposite Equal = NotEqual
opposite AL = error "There is no implementation for branching never. There is no opposite of AL"
