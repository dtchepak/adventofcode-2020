module Aoc.Day08
  ( part1,
    part2,
  )
where

import Control.Monad ((<=<))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Vector ((!?), (//))
import qualified Data.Vector as V

type InstrIndex = Int

type Acc = Int

data ProgramResult
  = Loop ProgramState
  | InvalidIndex Int
  | UnrecognisedOp Text
  | ParseError Text
  | Finished ProgramState
  deriving (Show, Eq)

data ProgramState = ProgramState
  { instr :: InstrIndex,
    acc :: Acc,
    history :: Set InstrIndex
  }
  deriving (Show, Eq)

data Instruction = Instruction Text Int
  deriving (Show, Eq)

readInstruction :: Text -> Either ProgramResult Instruction
readInstruction code =
  let (cmd, arg) = T.drop 1 <$> T.breakOn " " code
      readSignedInt = fmap fst . signed decimal
   in bimap
        (ParseError . T.append ("(" <> cmd <> ", " <> arg <> ")" <> ": ") . T.pack)
        (Instruction cmd)
        (readSignedInt arg)

readProgram :: Text -> Either ProgramResult (V.Vector Instruction)
readProgram =
  fmap V.fromList . traverse readInstruction . T.lines

step :: (Acc -> Acc) -> (InstrIndex -> InstrIndex) -> ProgramState -> ProgramState
step op next (ProgramState i a h) = ProgramState (next i) (op a) (i `Set.insert` h)

stepProgram :: V.Vector Instruction -> ProgramState -> Either ProgramResult ProgramState
stepProgram v state =
  let run "nop" _ = pure (step id succ state)
      run "acc" i = pure (step (+ i) succ state)
      run "jmp" i = pure (step id (+ i) state)
      run op _ = Left . UnrecognisedOp $ op
      lookupInstr i
        | i `Set.member` history state = Left . Loop $ state
        | i == V.length v = Left . Finished $ state
        | i < 0 || i > V.length v = Left . InvalidIndex $ i
        | otherwise = maybe (Left . InvalidIndex $ i) Right (v !? i)
   in lookupInstr (instr state) >>= \(Instruction i arg) -> run i arg

runToResult :: V.Vector Instruction -> ProgramResult
runToResult program =
  let run v = either id (run v) . stepProgram program
   in run program (ProgramState 0 0 Set.empty)

part1 :: Text -> Either ProgramResult (InstrIndex, Acc)
part1 =
  let getResult (Loop state) = pure (instr state, acc state)
      getResult other = Left other
   in (=<<) (getResult . runToResult) . readProgram

findUncorruptedResult :: V.Vector Instruction -> Maybe ProgramResult
findUncorruptedResult =
  let updateJmp idx (Instruction "jmp" arg) = Just [(idx, Instruction "nop" arg)]
      updateJmp _ _ = Nothing
      possiblePrograms original = (original //) <$> V.imapMaybe updateJmp original
   in V.foldr
        ( \program next -> case runToResult program of
            r@(Finished _) -> Just r
            _ -> next
        )
        Nothing
        . possiblePrograms

part2 :: Text -> Maybe (InstrIndex, Acc)
part2 =
  let getResult (Finished state) = pure (instr state, acc state)
      getResult _ = Nothing
   in (=<<) (getResult <=< findUncorruptedResult)
        . either (const Nothing) pure
        . readProgram