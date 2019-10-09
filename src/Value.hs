-- | A value that can be stored in the stack
module Value where
    
import Text.Read ( readMaybe )

-- | A value that the stack contains.
data Value = Number Float | Operator Char

-- | For printing out the value.
instance Show Value where
    show (Number x) = show x
    show (Operator x) = [x]
    
-- | Maybe reads a string into a value.
readValueMaybe :: String -> Maybe Value
readValueMaybe x
    | x == "+" = Just $ Operator '+'
    | x == "-" = Just $ Operator '-' 
    | x == "*" = Just $ Operator '*'
    | x == "/" = Just $ Operator '/'
readValueMaybe x =
    case readNum of
        (Just y) -> (Just $ Number y)
        Nothing -> Nothing
    where
        readNum = readMaybe x :: Maybe Float