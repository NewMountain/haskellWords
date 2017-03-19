module Lib
    ( grid
    , languages
    , outputGrid
    , solveGrid
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid = putStrLn . formatGrid

formatGrid :: Grid -> String
formatGrid = unlines 

gridBothWays :: Grid -> Grid
gridBothWays grid' = grid' ++ fmap reverse grid'

getLines :: Grid -> [String]
getLines grid' = horiVertDiag
    where horiVertDiag = hori ++ vert ++ sE ++ nE
          hori = gridBothWays grid'
          vert = gridBothWays $ transpose grid'
          sE = gridBothWays $ transpose $ skew grid'
          nE = gridBothWays $ transpose $ skew $ reverse grid'

findWord :: Grid -> String -> Maybe String
findWord grid' word = if found then Just word else Nothing
    where found = or $ findWordInLine word <$> getLines grid'

findWords :: Grid -> [String] -> [String]
findWords grid' words' = catMaybes foundWords
    where foundWords = fmap (findWord grid') words'

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (fmap indent ls)
    where indent line = '_' : line

solveGrid :: [String]
solveGrid = findWords grid languages



grid :: Grid
grid = [ "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

languages :: [String]
languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]
