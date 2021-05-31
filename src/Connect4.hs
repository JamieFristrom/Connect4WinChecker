module Connect4
    ( 
    FirstName(..),
    LastName(..),
    TotalName(..),
    nameTest,
    render,
    sublists,
    addToFirstList,
    addToNthList,
    boardHeight,
    getPiece,
    wonColumn,
    bestLength,
    bestRunLength,
    verticalWin,
    getPieceFromColumn,
    extractRow,
    addToFirstCount,
    horizontalLengths,
    horizontalTraversal,
    wonLine,
    winCheckFuncs,
    wonRow,
    wonAnyRowBelowN,
    runLengths,
    horizontalWin,
    makeMove,
    makeBoard,
    initialBoard,
    playGame
    ) where

import Data.List
import Data.Maybe
import C4Color
import System.Random

type C4Board = [[C4Color]]

data DivisionResult = DivisionByZero | Success Double
    deriving (Show)

type Column = [Maybe C4Color]

type Board = [Column]

type Row = [Maybe C4Color]

type RunLengths = [(Int,Maybe C4Color)]

-- 2d array 
type Array2D a = [[a]]

addToFirstList :: [[a]] -> a -> [[a]]
addToFirstList [] newElement = [[newElement]]  -- is this "correct"? There is no list to add to, should we throw an error instead?
addToFirstList (firstList:remainingLists) newElement = (newElement:firstList):remainingLists


addToNthList :: [[a]] -> Int -> a -> [[a]]
addToNthList lists ndx newElement = (take ndx lists) ++ (newElement:(lists !! ndx)) : (drop (ndx+1) lists)

-- divides into lists of same color
sublists :: Column -> [[Maybe C4Color]]
sublists [] = [[]]
sublists (checker:[]) = [[checker]]
sublists (checker:checkers) = if checker == head checkers then addToFirstList (sublists checkers) checker
                                                          else [checker]:sublists checkers


-- this assumes there are lists & that each list is nonzero & same color, 
-- I should have just done this more directly
sublistLengths :: [[Maybe C4Color]] -> [(Int,Maybe C4Color)]
sublistLengths splitlists = map (lengthAndColor) splitlists
                                where lengthAndColor xs = (length xs, head xs)


bestLength :: [(Int,Maybe C4Color)] -> (Int,Maybe C4Color)
bestLength counts = foldr1 takeBest counts
                      where takeBest a b = if fst a > fst b then a else b


-- todo; need to eliminate Nothing runs
wonColumn :: Column -> Maybe C4Color
wonColumn column = let best = bestLength (sublistLengths (sublists column ))
                    in if fst best >= 4 then snd best
                                        else Nothing                


verticalWin :: Board -> Maybe C4Color
verticalWin board = let result = find isJust (map wonColumn (board))
                        in if isNothing result then Nothing else fromJust result  -- yuck


bestRunLength :: [(Int,Maybe C4Color)] -> (Int,Maybe C4Color)
bestRunLength counts = let nonEmptyLists = filter isntEmpty counts
                        in if nonEmptyLists == [] then (0,Nothing)
                                                  else maximum nonEmptyLists
                        where isntEmpty x = isJust (snd x)


getPieceFromColumn :: Int -> Column -> Maybe C4Color
getPieceFromColumn rowNum column = if rowNum < length column then column!!rowNum else Nothing


getPiece :: Board -> Int -> Int -> Maybe C4Color
getPiece board colN rowN = let column = board !! colN   -- crash if you're out of bounds, you made a mistake
                            in if rowN >= length column then Nothing
                                                        else column !! rowN



extractRow :: Board -> Int -> Row
extractRow board rowNum = map (getPieceFromColumn rowNum) board


addToFirstCount :: [(Int,Maybe C4Color)] -> [(Int, Maybe C4Color)]
addToFirstCount ((num,col):xs) = (num+1,col):xs


horizontalLengths :: Row -> [(Int,Maybe C4Color)]
horizontalLengths [] = []
horizontalLengths (x:[]) = [(1,x)]
horizontalLengths (x:xs) = if x == (head xs) then addToFirstCount (horizontalLengths xs)
                                             else (1,x):(horizontalLengths xs)

horizontalTraversal :: (Int, Int)->(Int, Int)
horizontalTraversal (x0, y0) = (x0+1, y0) 

verticalTraversal :: (Int, Int)->(Int, Int)
verticalTraversal (x0, y0) = (x0, y0+1)

diagonalNWTraversal :: (Int, Int)->(Int, Int)
diagonalNWTraversal (x0, y0) = (x0-1,y0+1)

diagonalNETraversal :: (Int, Int)->(Int, Int)
diagonalNETraversal (x0, y0) = (x0+1,y0+1)


incrementFirstRun :: [(Int,Maybe C4Color)] -> [(Int,Maybe C4Color)]
incrementFirstRun ((headRunCount,headRunFlavor):restOfRuns) = (headRunCount+1,headRunFlavor):restOfRuns


runLengths :: Board -> (Int, Int) -> ((Int,Int)->(Int,Int)) -> [(Int,Maybe C4Color)]
runLengths board (x, y) traversalFunc 
    | x < 0                  = []
    | x >= length board      = []
    | y < 0                  = []
    | y >= boardHeight board = []
    | otherwise = let runs = runLengths board (traversalFunc (x,y)) traversalFunc
                      piece = getPiece board x y
                   in if runs == [] then [(1,piece)]
                                    else if piece == snd (head runs) then incrementFirstRun runs
                                                                     else (1,piece):runs



wonLine :: ((Int,Int)->(Int,Int)) -> (Int, Int) -> Board -> Maybe C4Color
wonLine traversalFunc xy0 board = let (count,mcolor) = bestRunLength $ runLengths board xy0 traversalFunc
                                   in if count >= 4 then mcolor else Nothing
                                   

winCheckFuncs = (map (wonLine verticalTraversal) [ (x,0) | x <- [0..6] ] ) ++
                (map (wonLine diagonalNWTraversal) [ (x,0) | x <- [0..6] ]) ++
                (map (wonLine diagonalNETraversal) [ (x,0) | x <- [0..6] ]) ++
                (map (wonLine horizontalTraversal) [ (0,x) | x <- [0..6] ])


-- gross!!!!  guess I should find out what Haskell 2d array best practice is
wonRow :: Row -> Maybe C4Color
wonRow row = let (count,mcolor) = bestRunLength $ horizontalLengths row
                        in if count >= 4 then mcolor else Nothing

wonAnyRowBelowN :: Int -> Board -> Maybe C4Color
wonAnyRowBelowN 0 board = wonRow $ extractRow board 0
wonAnyRowBelowN row board = let result = wonRow $ extractRow board row
                             in if isJust result then result 
                                                 else wonAnyRowBelowN (row-1) board


boardHeight :: Board -> Int
boardHeight board = maximum $ map length board


horizontalWin :: Board -> Maybe C4Color
horizontalWin board = wonAnyRowBelowN (boardHeight board) board

--   let topRow = maximum $ map length board
--                           rowResult = find isJust $ map (wonRow . (extractRow board)) [0..topRow-1]
--                        in if isNothing rowResult then Nothing else fromJust rowResult  -- yuck

diagonalNWWin :: Board -> Maybe C4Color
diagonalNWWin board = Nothing  -- stub


diagonalNEWin :: Board -> Maybe C4Color
diagonalNEWin board = Nothing  -- stub


-- usually when composing maybe's we want Nothing to win, but in this case it's more of an I won _or_ you won means someone won
orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe Nothing b = b
orMaybe a _ = a  -- in case of a tie a wins



winFuncs = [horizontalWin, verticalWin, diagonalNWWin, diagonalNEWin]

-- this is the opposite of the way we usually compose maybe's - normally we and them


anyWin :: Board -> Maybe C4Color
anyWin board = foldr1 (orMaybe) $ map (applyToBoard board) winCheckFuncs
                                        where applyToBoard board winFunc = winFunc board


-- makes the assumption we haven't screwed up and 
makeMove :: Board -> Int -> C4Color -> Board
makeMove board colNum pcolor = addToNthList board colNum (Just pcolor)


initialBoard :: Int -> Board
initialBoard width = replicate width []


oppositeColor :: C4Color -> C4Color
oppositeColor color = if color == Red then Blue else Red


makeBoard :: Board -> C4Color -> [Int] -> Board
makeBoard board whoseTurn [] = board
makeBoard board whoseTurn (nextMove:remainingMoves) = let board' = makeMove board nextMove whoseTurn
                                                       in makeBoard board' (oppositeColor whoseTurn) remainingMoves

-- turns a list of moves into a win or not.
playGame :: Board -> C4Color -> [Int] -> Maybe C4Color
playGame board whoseTurn [] = Nothing  -- no moves left, and nobody must have won
playGame board whoseTurn (nextMove:remainingMoves) = 
    let board' = makeMove board nextMove whoseTurn 
    in if anyWin board' == Nothing then playGame board' (oppositeColor whoseTurn) remainingMoves
                                   else anyWin board'
    

playRandomGame :: Maybe C4Color
playRandomGame = do
    stdg
----------------------------------------------------------------
-- test on strictness of typing

newtype FirstName = FirstName String
    deriving (Show)

newtype LastName = LastName String
    deriving (Show)

newtype TotalName = TotalName (FirstName,LastName)
    deriving (Show)

nameTest :: LastName -> FirstName -> TotalName
nameTest lastName firstName = TotalName (firstName, lastName)

-- this fails to compile as one would hope
--nameTest2 :: LastName -> FirstName -> TotalName
--nameTest2 lastName firstName = TotalName (lastName, firstName)
render :: TotalName -> String
render (TotalName (FirstName first, LastName last)) = first ++ " " ++ last