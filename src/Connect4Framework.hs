module Connect4Framework
    ( 
    makeBoard,
    playGame,
    playRandomGame,
    testCase
    ) where

import Data.Maybe
import Data.Time
import qualified Data.Vector as Vec
import System.Random

import C4Color
import Connect4ListImpl


makeBoard :: Board -> C4Color -> [Int] -> Board
makeBoard board whoseTurn [] = board
makeBoard board whoseTurn (nextMove:remainingMoves) = let board' = makeMove board nextMove whoseTurn
                                                       in makeBoard board' (oppositeColor whoseTurn) remainingMoves

-- turns a list of moves into a win or not.
playGame :: Board -> C4Color -> [Int] -> (Maybe C4Color, Board)
playGame board whoseTurn [] = (Nothing, board)  -- no moves left, and nobody must have won
playGame board whoseTurn (nextMove:remainingMoves) = 
    let board' = makeMove board nextMove whoseTurn 
    in if anyWin board' == Nothing then playGame board' (oppositeColor whoseTurn) remainingMoves
                                   else (anyWin board', board')
    

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
    
