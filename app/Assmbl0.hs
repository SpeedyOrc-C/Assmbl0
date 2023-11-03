{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Assmbl0 where

import qualified Data.Map as Map

type Memory = Map.Map Int Int
type Registers = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

data State = State {
    memory :: Map.Map Int Int,
    registers :: Registers,
    pc :: Int,
    finished :: Bool
}

instance Show State where
    show :: State -> String
    show (State memory registers pc _) =
        "[PC] " ++ show pc ++ "\n" ++
        "[Memory] " ++ show memory ++ "\n" ++
        "[Registers] " ++ show registers ++ "\n"

data Instruction
    = ADD Register Register
    | SUB Register Register
    | LOAD Register Register
    | LOADC Register Int
    | STORE Register Register
    | COND Register Int
    | GOTO Int
    | FIN

data Register
    = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
    deriving (Eq, Ord, Enum)

type Program = [Instruction]

readReg :: Register -> Registers -> Int
readReg R0 (r,_,_,_,_,_,_,_,_,_) = r
readReg R1 (_,r,_,_,_,_,_,_,_,_) = r
readReg R2 (_,_,r,_,_,_,_,_,_,_) = r
readReg R3 (_,_,_,r,_,_,_,_,_,_) = r
readReg R4 (_,_,_,_,r,_,_,_,_,_) = r
readReg R5 (_,_,_,_,_,r,_,_,_,_) = r
readReg R6 (_,_,_,_,_,_,r,_,_,_) = r
readReg R7 (_,_,_,_,_,_,_,r,_,_) = r
readReg R8 (_,_,_,_,_,_,_,_,r,_) = r
readReg R9 (_,_,_,_,_,_,_,_,_,r) = r

writeReg :: Register -> Int -> Registers -> Registers
writeReg R0 r (_,r1,r2,r3,r4,r5,r6,r7,r8,r9) = (r,r1,r2,r3,r4,r5,r6,r7,r8,r9)
writeReg R1 r (r0,_,r2,r3,r4,r5,r6,r7,r8,r9) = (r0,r,r2,r3,r4,r5,r6,r7,r8,r9)
writeReg R2 r (r0,r1,_,r3,r4,r5,r6,r7,r8,r9) = (r0,r1,r,r3,r4,r5,r6,r7,r8,r9)
writeReg R3 r (r0,r1,r2,_,r4,r5,r6,r7,r8,r9) = (r0,r1,r2,r,r4,r5,r6,r7,r8,r9)
writeReg R4 r (r0,r1,r2,r3,_,r5,r6,r7,r8,r9) = (r0,r1,r2,r3,r,r5,r6,r7,r8,r9)
writeReg R5 r (r0,r1,r2,r3,r4,_,r6,r7,r8,r9) = (r0,r1,r2,r3,r4,r,r6,r7,r8,r9)
writeReg R6 r (r0,r1,r2,r3,r4,r5,_,r7,r8,r9) = (r0,r1,r2,r3,r4,r5,r,r7,r8,r9)
writeReg R7 r (r0,r1,r2,r3,r4,r5,r6,_,r8,r9) = (r0,r1,r2,r3,r4,r5,r6,r,r8,r9)
writeReg R8 r (r0,r1,r2,r3,r4,r5,r6,r7,_,r9) = (r0,r1,r2,r3,r4,r5,r6,r7,r,r9)
writeReg R9 r (r0,r1,r2,r3,r4,r5,r6,r7,r8,_) = (r0,r1,r2,r3,r4,r5,r6,r7,r8,r)

readMemory :: (Int, Memory) -> (Int, Memory)
readMemory (index, memory) =
    if index `Map.member` memory
    then (memory Map.! index, memory)
    else (0, Map.insert index 0 memory)

writeMemory :: (Int, Int, Memory) -> Memory
writeMemory (index, newValue, memory) =
    if index `Map.member` memory
    then Map.update (const $ Just newValue) index memory
    else Map.insert index newValue memory

initialRegisters :: Registers
initialRegisters = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

initialState :: State
initialState = State Map.empty initialRegisters 1 False

step :: Program -> State -> State
step [] _ = error "The program is empty."
step program state
    | pc < 1 || pc > length program = error "RTE: Program counter is out of bound."
    | otherwise = step' (instruction, state) 
    where
        (State _ _ pc _) = state
        instruction = program !! (pc - 1)

step' :: (Instruction, State) -> State

step' (GOTO n, State memory registers _ False) = State memory registers n False

step' (FIN, State memory registers pc False) = State memory registers pc True

step' (COND r n, State memory registers pc False) =
    State memory registers
        (if readReg r registers >= 0 then pc + 1 else n) False

step' (ADD r r', State memory registers pc False) =
    State memory registers' (pc + 1) False where
        registers' = writeReg r
            (readReg r registers + readReg r' registers) registers

step' (SUB r r', State memory registers pc False) =
    State memory registers' (pc + 1) False where
        registers' = writeReg r
            (readReg r registers - readReg r' registers) registers

step' (LOAD register indexRegister, State memory registers pc False) =
    State memory' registers' (pc + 1) False where
        index = readReg indexRegister registers
        ~(value, memory') = readMemory (index, memory)
        registers' = writeReg register value registers

step' (STORE register indexRegister, State memory registers pc False) =
    State memory' registers (pc + 1) False where
        index = readReg indexRegister registers
        value = readReg register registers
        memory' = writeMemory (index, value, memory)

step' (LOADC register value, State memory registers pc False) =
    State memory registers' (pc + 1) False where
        registers' = writeReg register value registers

step' (_, State _ _ _ True) = error "Finished program cannot step forward anymore."

execute :: Program -> [(Int, Int)] -> [State]
execute program memory = takeWhile (not.finished) $ 
    iterate (step program) (State (Map.fromList memory) initialRegisters 1 False)
