module Lib
    ( someFunc
    , crappygcd
--    , pop
--    , push
    , Stack
--    , stackManip
--    , W
    ) where

newtype State s a = State { runState :: s -> (a, s) }



type Stack = [Integer]

-- pop :: State Stack Int
-- pop = State $ \(x:xs) -> (x,xs)

-- push :: Int -> State Stack ()
-- push a = State $ \xs -> ((),a:xs)

-- stackManip :: State Stack Int
-- stackManip = do
--     push 3
--     a <- pop
--     pop
-- pop :: Stack -> (Integer, Stack)
-- pop (x:xs) = (x,xs)

-- push :: Integer -> Stack -> ((), Stack)
-- push x xs = ((),x:xs)

-- stackManip :: Stack -> (Integer, Stack)
-- stackManip stack = let ((),newStack1) = push 3 stack
--                        (a, newStack2) = pop newStack1
--                    in pop newStack2





crappygcd_guts :: Int -> Int -> Int -> Int
crappygcd_guts a b c 
    | c == 1    = c
    | otherwise = if a `mod` c == 0 && b `mod` c == 0 then c else crappygcd_guts a b (c-1)

crappygcd :: Int -> Int -> Int
crappygcd a b = crappygcd_guts a b $ min a b

-- gcd' :: Int -> Int -> Writer [String] Int  
-- gcd' a b  
--     | b == 0 = do  
--         tell ["Finished with " ++ show a]  
--         return a  
--     | otherwise = do  
--         tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
--         gcd' b (a `mod` b)  

-- type Food = String
-- type Price = Sum Int

-- addDrink :: Food -> (Food,Price)
-- addDrink "beans" = ("milk", Sum 25)
-- addDrink "jerky" = ("whiskey", Sum 99)
-- addDrink _ = ("beer", Sum 30)

-- applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
-- applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  

-- type KnightPos = (Int,Int)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- knightMoves :: KnightPos -> [KnightPos]  
-- knightMoves (c,r) = filter onBoard  
--     [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
--     ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
--     ]  
--     where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]  

-- moveKnight :: [KnightPos] -> [[KnightPos]]
-- moveKnight posList = do
--     let (c,r) = head posList
--     (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
--                 ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
--                 ]
--     guard (c' `elem` [1..8] && r' `elem` [1..8])
--     return (c',r'):posList

-- --in3 :: KnightPos -> [KnightPos]
-- --in3 start = return [start] >>= moveKnight >>= moveKnight >>= moveKnight

-- allChains :: [KnightPos] -> [[KnightPos]]
--     map 

-- moveChain :: KnightPos -> [KnightPos] -> [KnightPos]
-- moveChain target steplist
--     | lastStep == (head steplist) = steplist
--     | length steplist > 3 = []
--     | (filter (\(step:steps) -> target==step)
--         allChains knightMoves (fst step,snd step)
        
