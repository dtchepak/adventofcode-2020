module Aoc.Day08
  ( runProgram,
    part1,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Vector ((!?))
import qualified Data.Vector as V

type InstrIndex = Int

type Acc = Int

data ProgramError
  = Loop ProgramState
  | InvalidIndex Int
  | UnrecognisedOp Text
  | ParseError Text
  deriving (Show, Eq)

data ProgramState = ProgramState
  { instr :: InstrIndex,
    acc :: Acc,
    history :: Set InstrIndex
  }
  deriving (Show, Eq)

data Instruction = Instruction Text Int
  deriving (Show, Eq)

readInstruction :: Text -> Either ProgramError Instruction
readInstruction code =
  let (cmd, arg) = T.drop 1 <$> T.breakOn " " code
      readSignedInt = fmap fst . signed decimal
   in bimap
        (ParseError . T.append ("(" <> cmd <> ", " <> arg <> ")" <> ": ") . T.pack)
        (Instruction cmd)
        (readSignedInt arg)

readProgram :: Text -> Either ProgramError (V.Vector Instruction)
readProgram =
  fmap V.fromList . traverse readInstruction . T.lines

step :: (Acc -> Acc) -> (InstrIndex -> InstrIndex) -> ProgramState -> ProgramState
step op next (ProgramState i a h) = ProgramState (next i) (op a) (i `Set.insert` h)

stepProgram :: V.Vector Instruction -> ProgramState -> Either ProgramError ProgramState
stepProgram v state =
  let run "nop" _ = pure (step id succ state)
      run "acc" i = pure (step (+ i) succ state)
      run "jmp" i = pure (step id (+ i) state)
      run op _ = Left . UnrecognisedOp $ op
      lookupInstr i
        | i `Set.member` history state = Left . Loop $ state
        | i < 0 || i >= V.length v = Left . InvalidIndex $ i
        | otherwise = maybe (Left . InvalidIndex $ i) Right (v !? i)
   in lookupInstr (instr state) >>= \(Instruction i arg) -> run i arg

runToError :: V.Vector Instruction -> ProgramError
runToError program =
  let run v = either id (run v) . stepProgram program
   in run program (ProgramState 0 0 Set.empty)

runProgram :: Text -> Either ProgramError (InstrIndex, Acc)
runProgram =
  let getLoopError (Loop state) = pure (instr state, acc state)
      getLoopError other = Left other
   in (=<<) (getLoopError . runToError) . readProgram

part1 :: Text -> Either ProgramError (InstrIndex, Acc)
part1 = runProgram