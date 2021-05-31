-- Elegant but much slower (5x for board width of 500) as I originally suspected

-- comments for non Haskellers
module Connect4ListImpl
    (
    Checker(..),
    Board(..),
    isCheckerInColumn,
    isCheckerInRow,
    isCheckerInNEDiagonal,
    isCheckerInNWDiagonal,
    getTheNWDiagonal,
    getTheNEDiagonal,
    runLength,
    runLengths,
    longestRunLength,
    maxBoardDimension,
    boardWidth,
    boardHeight,
    getBoardLine,
    columnHeight,    
    makeMove,
    anyLineWin,
    anyWin,
    initialBoard
    ) where

import Data.Sort
import C4Color
-- basically a struct
data Checker = Checker { mColor :: C4Color
                       , mColN :: Int
                       , mRowN :: Int
                       } deriving (Show)

type Board = [Checker] -- basically a type alias - Board is a list of Checkers


-- takes a checker and a column, returns whether the checker is in it
isCheckerInColumn :: Int -> Checker -> Bool
isCheckerInColumn colN checker = mColN checker == colN


-- frex, row 1, col 0 is in the col 1 nw diagonal . y = k - x; rowN = k - colN
isCheckerInNWDiagonal :: Int -> Checker -> Bool
isCheckerInNWDiagonal colN checker = mRowN checker == colN - mColN checker


-- 3,0 - 4,1 - 5,2 -> x = y + k
-- frex row 1, col 2 is in the col 1 ne diagonal. x = k + y; colN = k + rowN
isCheckerInNEDiagonal :: Int -> Checker -> Bool
isCheckerInNEDiagonal colN checker = mColN checker == colN + mRowN checker


isCheckerInRow :: Int -> Checker -> Bool
isCheckerInRow rowN checker = mRowN checker == rowN


-- get the height of a column; takes the Board and a column, returns the height
-- works by filtering the list down to the checkers that are in the column, then 
-- just counts them, so 
-- makes assumption that we haven't made any mistakes - no two checkers in same spot,
-- no floating checkers. An alternative, a list of lists for board state, is more accurate
-- but harder to work with 
columnHeight :: Board -> Int -> Int
columnHeight gameState colN = length $ filter (isCheckerInColumn colN) gameState  -- $ is a ( that you don't have to remember to close


-- check the diagonal three brothers gone come on
getTheNWDiagonal :: Board -> Int -> [Checker]
getTheNWDiagonal gameState colN = sortOn (mColN) $ filter (isCheckerInNWDiagonal colN) gameState


getTheNEDiagonal :: Board -> Int -> [Checker]
getTheNEDiagonal gameState colN = sortOn (mColN) $ filter (isCheckerInNEDiagonal colN) gameState


-- ignores color; filter first
runLength :: [Checker] -> Int
runLength [] = 0
runLength (checker:[]) = 1
runLength (checkerA:checkerB:checkers) = if abs( mRowN checkerA - mRowN checkerB ) <= 1 &&
                                            abs( mColN checkerA - mColN checkerB ) <= 1 
                                            then 1 + (runLength (checkerB:checkers))
                                            else 1

-- ignores color; filter first
runLengths :: [Checker] -> [Int]
runLengths [] = []
runLengths checkers = runLength checkers : runLengths (tail checkers) 


-- not as flexible as maximum
safeMaximum :: [Int] -> Int
safeMaximum values 
    | length values == 0  = 0
    | otherwise             = maximum values


longestRunLength :: [Checker] -> C4Color -> Int
longestRunLength [] ccolor = 0
longestRunLength checkers ccolor = safeMaximum $ runLengths filteredCheckers 
                                    where filteredCheckers = filter (\checker -> mColor checker == ccolor) checkers


maxBoardDimension :: (Checker -> Int) -> Board -> Int
maxBoardDimension dimensionFn [] = 0
maxBoardDimension dimensionFn gameState = safeMaximum $ map (dimensionFn) gameState


boardWidth :: Board -> Int
boardWidth = maxBoardDimension mColN


boardHeight :: Board -> Int
boardHeight = maxBoardDimension mRowN


-- allows us to specify the function that detects whether a given piece is in the line
-- so flexible across diagonal, vertical, and row
getBoardLine :: (Int -> Checker -> Bool) -> Board -> Int -> [Checker]
getBoardLine isCheckerInLineFn gameState colN = sortOn (mColN) $ filter (isCheckerInLineFn colN) gameState


lineWin :: (Int -> Checker -> Bool) -> Board -> C4Color -> Int -> Bool
lineWin isCheckerInLineFn gameState color colN = 4 <= longestRunLength (getBoardLine isCheckerInLineFn gameState colN) color


anyLineWin :: (Int -> Checker -> Bool ) -> Board -> C4Color -> Bool
anyLineWin isCheckerInLineFn gameState ccolor = foldr1 (||) $ map (lineWin isCheckerInLineFn gameState ccolor) [0..(boardWidth gameState)]


lineFns :: [(Int -> Checker -> Bool )]
lineFns = [isCheckerInColumn, isCheckerInRow, isCheckerInNEDiagonal, isCheckerInNWDiagonal]


colorWon :: Board -> C4Color -> Bool
colorWon gameState ccolor = foldr1 (||) $ map (\fn -> fn gameState ccolor) $ map (anyLineWin) lineFns


anyWin :: Board -> Maybe C4Color
anyWin board = if colorWon board Red then Just Red 
                                     else if colorWon board Blue then Just Blue
                                                                 else Nothing


-- add a new checker to the game state
makeMove :: Board -> Int -> C4Color -> Board
makeMove gameState colN color = Checker { mColor = color
                                        , mColN = colN
                                        , mRowN = columnHeight gameState colN 
                                        }:gameState

initialBoard :: Int -> Board
initialBoard _ = []


skipSpaceTestCase = [Checker {mColor=Red,mColN=0,mRowN=0}, Checker {mColor=Red,mColN=2,mRowN=0}]