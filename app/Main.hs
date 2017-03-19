module Main where

import Lib

main :: IO ()
main = do
    putStrLn (formatGrid grid)
    print (solveGrid grid languages)
