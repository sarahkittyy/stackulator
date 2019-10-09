-- | Defines all the values that are stored in the stackulator's stack.
module Values where
    
import Text.Read ( readMaybe )

-- | A value that can be stored in the stack
data Value = Operator Char | Number Float | Evaluate | Pop

-- | For showing those values.
instance Show Value where
    show (Operator x) = [x]
    show (Number x) = show x
    show Evaluate = "eval"
    
-- | Converts a string to a value appendable to the stack.
fromInput :: String -> Maybe Value
fromInput x
    | x == "+" = Just $ Operator '+'
    | x == "-" = Just $ Operator '-'
    | x == "*" = Just $ Operator '*'
    | x == "/" = Just $ Operator '/'
    | x == "eval" = Just Evaluate
    | x == "pop" = Just Pop
fromInput x =
    let num = readMaybe x :: Maybe Float
    in
        case num of
            Just x -> Just (Number x)
            Nothing -> Nothing