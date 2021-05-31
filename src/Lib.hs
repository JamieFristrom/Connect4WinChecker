{-# LANGUAGE InstanceSigs #-}

module Lib
    ( someFunc
    , crappygcd
    , composition
    , TurnstileOutput(..)
    , TurnstileInput(..)
    , TurnstileState(..)
    , State(..)
    , coin
    , push
    , coinSX
    , pushSX
    , monday
    , tuesday
    , regularPerson
    , distractedPerson
    , hastyPerson
    , luckyPair
    , regularPersonSX
    , distractedPersonSX
    , hastyPersonSX
    , luckyPairSX
    , turnSX
    , testTurnstile
    , get
    , put
    , modify
    , gets
    -- , tick
    -- , pop
    -- , push
    -- , Stack(..)
    -- , stackManip
--    , W
    ) where

import Control.Monad

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Monad (State s) where
    return :: a -> State s a  -- illegal in standard haskell
    return x = state ( \ s -> (x, s) )

    (>>=) :: State s a -> (a -> State s b) -> State s b
    stateXform0 >>= valueToStateXformFunc = stateXformResult where
        stateXform0' = runState stateXform0
        valueToStateXform = runState . valueToStateXformFunc  -- composing the value to state func with a state transition
        stateXformResult' state0 = (yResult, state2) where
            (xResult, state1) = stateXform0' state0
            (yResult, state2) = valueToStateXform xResult state1
        stateXformResult = state stateXformResult'

put :: s -> State s ()
put newState = state $ \_ -> ((), newState)

get :: State s s
get = state $ \s -> (s, s) 

modify :: (s -> s) -> State s ()
modify mutator = state $ \s -> ((), mutator s)

gets :: (s -> a) -> State s a
gets mutator = state $ \s -> (mutator s, s)


composition :: (s -> (a,s)) -> (a -> (s -> (b, s ))) -> s -> (b,s)
composition f g = \state0 -> let ( fResult, fStateResult ) = f state0 
                              in g fResult fStateResult


data TurnstileState = Locked | Unlocked
    deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
    deriving (Eq, Show)

data TurnstileInput = Coin | Push
    deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Unlocked = (Open, Locked)
push Locked   = (Tut, Locked)
--import Control.Monad.Trans.State

coinSX :: State TurnstileState TurnstileOutput
coinSX = do
    s0 <- get
    put Unlocked
    return Thank

pushSX :: State TurnstileState TurnstileOutput
pushSX = state push

turnSX :: TurnstileInput -> State TurnstileState TurnstileOutput
turnSX = state . turn where
    turn Coin _        = (Thank, Unlocked)
    turn Push Unlocked = (Open, Locked)
    turn Push Locked   = (Tut, Locked)


monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 = 
    let (a1, s1) = coin s0
        (a2, s2) = push s1
        (a3, s3) = push s2
        (a4, s4) = coin s3
        (a5, s5) = push s4
    in ([a1, a2, a3, a4, a5], s5)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 =
    let (a1, s1) = regularPerson s0
        (a2, s2) = hastyPerson s1
        (a3, s3) = distractedPerson s2
        (a4, s4) = hastyPerson s3
    in ( a1++a2++a3++a4, s4 )

regularPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
regularPerson s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
    in ([a1, a2], s2)

regularPersonSX :: State TurnstileState [TurnstileOutput] 
regularPersonSX = sequence [ coinSX, pushSX ]

distractedPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
distractedPerson s0 =
    let (a1, s1) = coin s0
    in ([a1], s1)

distractedPersonSX :: State TurnstileState [TurnstileOutput] 
distractedPersonSX = sequence [ coinSX ]

hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
hastyPerson s0 = 
    let (a1, s1) = push s0
    in if a1==Tut then
        let (as2, s2) = regularPerson s1
        in (a1:as2, s2) 
    else 
        ([a1],s1)

hastyPersonSX :: State TurnstileState [TurnstileOutput] 
hastyPersonSX = do
    pushSX >>= ( \thingy -> (if thingy == Tut then regularPersonSX else sequence []) 
                                >>= ( \lastThingy -> return (thingy:lastThingy) ) )
                                             


luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair isRegularPerson s0 = 
    let (a1, s1) = if isRegularPerson then regularPerson s0 else distractedPerson s0
        (a2, s2) = push s1
    in (a2 == Open, s2)


luckyPairSX :: Bool -> State TurnstileState Bool
luckyPairSX isRegularPerson = do
    if isRegularPerson then regularPersonSX else distractedPersonSX
    a2 <- pushSX
    return (a2 == Open)


testTurnstile :: State TurnstileState Bool
testTurnstile = do
  originalState <- get
  put Locked
  check1 <- pushSX
  coinSX
  check1' <- get
  put Unlocked
  check2 <- pushSX
  coinSX
  check2' <- get
  put originalState
  return (check1 == Tut && check1' == Unlocked && check2 == Open && check2' == Unlocked)

--newtype State s a = State { runState :: s -> (a, s) }

-- tick :: State Int Int
-- tick = do
--     n <- get
--     put (n+1)
--     return n

-- type Stack = [Int]

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
        
