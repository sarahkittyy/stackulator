-- | Defines all the values that are stored in the stackulator's stack.
module Values where

-- | A value that can be stored in the stack
data Value = Operator Char | Number Float

-- | For showing those values.
instance Show Value where
    show (Operator x) = "x"
    show (Number x) = show x