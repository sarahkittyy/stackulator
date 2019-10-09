-- | Defines all the values that are stored in the stackulator's stack.
module Values where

-- | A value that can be stored in the stack
data Value = Operator Char | Number Float | Evaluate

-- | For showing those values.
instance Show Value where
    show (Operator x) = "x"
    show (Number x) = show x
    show Evaluate = "eval"
    
-- | Converts a string to a value appendable to the stack.
fromInput :: String -> Value
fromInput x
    | x == "+" = Operator '+'
    | x == "-" = Operator '-'
    | x == "*" = Operator '*'
    | x == "/" = Operator '/'
    | x == "eval" = Evaluate
fromInput x = Number (read x :: Float)