module Example where

import Assmbl0

p0 :: [Instruction]
p0 = [
    LOADC R1 100,
    LOADC R2 101,
    LOADC R3 130,
    LOAD R1 R1,
    LOAD R2 R2,
    SUB R1 R2,
    COND R1 11,
    ADD R1 R2,
    STORE R1 R3,
    FIN,
    STORE R2 R3,
    FIN
    ]

m0 :: [(Int, Int)]
m0 = [
    (100, 200),
    (101, 201)
    ]

m2 :: [(Int, Int)]
m2 = [
    (100, 201),
    (101, 200)
    ]