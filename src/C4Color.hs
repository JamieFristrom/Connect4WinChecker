module C4Color ( 
    C4Color(..),
    oppositeColor
     ) where

data C4Color = Red | Blue
    deriving (Eq, Show, Ord)  -- Ord so I can use maximum. Needed a tie-breaker, don't care.

oppositeColor :: C4Color -> C4Color
oppositeColor color = if color == Red then Blue else Red

