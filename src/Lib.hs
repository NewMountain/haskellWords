module Lib
    ( grid
    , languages
    , solveGrid
    , formatGrid 
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines 

gridBothWays :: Grid -> Grid
gridBothWays grid' = grid' ++ fmap reverse grid'

diagonalize :: Grid -> Grid
diagonalize = gridBothWays . transpose . skew

getLines :: Grid -> [String]
getLines grid' = hori ++ vert ++ sE ++ nE
    where hori = gridBothWays grid'
          vert = gridBothWays $ transpose grid'
          sE =  diagonalize grid'
          nE = diagonalize $ reverse grid'

findWord :: Grid -> String -> Maybe String
findWord grid' word = if found then Just word else Nothing
    where found = or $ findWordInLine word <$> getLines grid'

findWords :: Grid -> [String] -> [String]
findWords grid' words' = catMaybes foundWords
    where foundWords = findWord grid' <$> words'

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (fmap indent ls)
    where indent line = '_' : line

solveGrid :: Grid -> [String] -> [String]
solveGrid = findWords

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
