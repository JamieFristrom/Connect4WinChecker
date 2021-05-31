module Connect4ArrImpl
    ( 
    Board(..),
    addToFirstList,
    addToNthColumn,
    boardHeight,
    getPiece,
    bestLength,
    bestRunLength,
    addToFirstCount,
    horizontalTraversal,
    wonLine,
    winCheckFuncs,
    runLengths,
    anyWin,
    makeMove,
    initialBoard
    ) where

import Data.Maybe
import Data.Time
import qualified Data.Vector as Vec
import C4Color
import System.Random





type Row = [Maybe C4Color]

type RunLengths = [(Int,Maybe C4Color)]

-- xTake = Vec.take
-- xDrop = Vec.drop
-- xLength = Vec.length
-- xCons = Vec.cons
-- xHead = Vec.head
-- xFold1 = Vec.foldr1
-- xFilter = Vec.filter
-- --xMaximum = Vec.maximum
-- xMap = Vec.map
-- xReplicate = Vec.replicate

xTake = Prelude.take
xDrop = Prelude.drop
xLength = Prelude.length
--xCons = cons
xHead = Prelude.head
--xFold1 = Prelude.foldr1
xFilter = Prelude.filter
--xMaximum = Prelude.maximum
xMap = Prelude.map
xReplicate = Prelude.replicate

-- list Column
type Column = [Maybe C4Color]
emptyColumn = []

yTake = Prelude.take
yDrop = Prelude.drop
yLength = Prelude.length
yCons = (:)
yHead = Prelude.head
--yFold1 = Prelude.foldr1
yFilter = Prelude.filter
--yMaximum = Prelude.maximum
yMap = Prelude.map
yReplicate = Prelude.replicate

-- vector Column
-- type Column = Vec.Vector (Maybe C4Color)
-- emptyColumn :: Column
-- emptyColumn = Vec.empty


-- yTake = Vec.take
-- yDrop = Vec.drop
-- yLength = Vec.length
-- yCons = Vec.cons
-- yHead = Vec.head
-- --yFold1 = Vec.foldr1
-- yFilter = Vec.filter
-- --yMaximum = Vec.maximum
-- yMap = Vec.map
-- yReplicate = Vec.replicate

-- list of lists impl

type Board = [Column]

--type Board = Vec.Vector Column


addToFirstList :: [[a]] -> a -> [[a]]
addToFirstList [] newElement = [[newElement]]  -- is this "correct"? There is no list to add to, should we throw an error instead?
addToFirstList (firstList:remainingLists) newElement = (newElement:firstList):remainingLists


-- addToNthList :: [[a]] -> Int -> a -> [[a]]
-- addToNthList lists ndx newElement = (take ndx lists) ++ (newElement:(lists !! ndx)) : (drop (ndx+1) lists)


-- Vector version 
-- addToNthColumn :: Board -> Int -> Maybe C4Color -> Board
-- addToNthColumn board colN checker = (xTake colN board) Vec.++ xCons (checker:(board Vec.! colN)) (xDrop (colN+1) board)

-- List version
addToNthColumn :: Board -> Int -> Maybe C4Color -> Board
addToNthColumn board colN checker = (xTake colN board) ++ (checker `yCons` (board !! colN)):(xDrop (colN+1) board)


bestLength :: [(Int,Maybe C4Color)] -> (Int,Maybe C4Color)
bestLength counts = foldr1 takeBest counts
                      where takeBest a b = if fst a > fst b then a else b


bestRunLength :: [(Int,Maybe C4Color)] -> (Int,Maybe C4Color)
bestRunLength counts = let nonEmptyLists = filter isntEmpty counts
                        in if nonEmptyLists == [] then (0,Nothing)
                                                  else maximum nonEmptyLists
                        where isntEmpty x = isJust (snd x)

-- board is a vector of columns version
-- getPiece :: Board -> Int -> Int -> Maybe C4Color
-- getPiece board colN rowN = let column = board Vec.! colN   -- crash if you're out of bounds, you made a mistake
--                             in if rowN >= yLength column then Nothing
--                                                          else column !! rowN

-- board is a list of columns version
getPiece :: Board -> Int -> Int -> Maybe C4Color
getPiece board colN rowN = let column = board !! colN   -- crash if you're out of bounds, you made a mistake
                            in if rowN >= yLength column then Nothing
                                                         else column !! (yLength column - rowN - 1)

addToFirstCount :: [(Int,Maybe C4Color)] -> [(Int, Maybe C4Color)]
addToFirstCount ((num,col):xs) = (num+1,col):xs


-- horizontalLengths :: Row -> [(Int,Maybe C4Color)]
-- horizontalLengths [] = []
-- horizontalLengths (x:[]) = [(1,x)]
-- horizontalLengths (x:xs) = if x == (head xs) then addToFirstCount (horizontalLengths xs)
--                                              else (1,x):(horizontalLengths xs)

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
    | x >= xLength board      = []
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


boardHeight :: Board -> Int
boardHeight board = maximum $ xMap yLength board


-- usually when composing maybe's we want Nothing to win, but in this case it's more of an I won _or_ you won means someone won
orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe Nothing b = b
orMaybe a _ = a  -- in case of a tie a wins


anyWin :: Board -> Maybe C4Color
anyWin board = foldr1 (orMaybe) $ map (applyToBoard board) winCheckFuncs
                                        where applyToBoard board winFunc = winFunc board


-- makes the assumption we haven't screwed up and 
makeMove :: Board -> Int -> C4Color -> Board
makeMove board colNum pcolor = addToNthColumn board colNum (Just pcolor)


initialBoard :: Int -> Board
initialBoard width = xReplicate width emptyColumn


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
    

testCase = playGame (initialBoard 7) Red [1,2,1,3,1,4,1,5] 

-- benchmark on my laptop for playRandomGame 1 
-- benchmark for using a list of lists = 8.033, 8.85, 9.3, 9.0

-- bencharmk for using a Vector of lists 10.7, 13.4, 13.2
-- benchmark for using a list of Vectors = 8.24, 8.36, 8.53

playRandomGame :: Int -> Int -> IO ()
playRandomGame seed width = do
    putStrLn "Starting"
    startTime <- getCurrentTime
    putStrLn $ "Start: " ++ show startTime 
    let randoms' = randoms (mkStdGen seed) :: [Int]
        columnNums = map (`mod` width) randoms' 
        result = playGame (initialBoard width) Red columnNums
    putStrLn $ show result  -- need to use it to evaluate it
    endTime <- getCurrentTime
    putStrLn $ "End: " ++ show endTime ++ " Total: " ++ show (diffUTCTime endTime startTime)
    
