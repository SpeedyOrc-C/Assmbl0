module Main where

import Assmbl0
import Example

import Data.Foldable (traverse_)

executeWithPrint :: Program -> [(Int, Int)] -> IO ()
executeWithPrint program memory = traverse_ print $ execute program memory

main :: IO ()
main = executeWithPrint p0 m0
