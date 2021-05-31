module C4Color ( C4Color(..) ) where

data C4Color = Red | Blue
    deriving (Eq, Show, Ord)  -- Ord so I can use maximum. Needed a tie-breaker, don't care.