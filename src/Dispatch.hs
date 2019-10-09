-- | Defines all the values that are stored in the stackulator's stack.
module Dispatch
( dispatch
) where
    
import Stack
import Value

-- | Redirects the flow depending on the input string
dispatch :: String -> [Value] -> Either String [Value]
dispatch "pop" s = Right $ popStack s
dispatch input stack =
    case readVal of
        (Just (Number y)) -> Right $ pushStack (Number y) stack
        (Just (Operator x)) -> let applied = applyOperator (Operator x) stack
                               in
                                    case applied of
                                        Left e -> Left e
                                        Right stack -> Right stack
        _ -> Left "Unknown Input"
                                        
    where
        readVal = readValueMaybe input
        
-- | Applies the operator to the given stack
applyOperator :: Value -> [Value] -> Either String [Value]
applyOperator (Operator '+') ((Number a):(Number b):rest) = Right $ (Number $ a + b):rest
applyOperator (Operator '-') ((Number a):(Number b):rest) = Right $ (Number $ b - a):rest
applyOperator (Operator '*') ((Number a):(Number b):rest) = Right $ (Number $ a * b):rest
applyOperator (Operator '/') ((Number a):(Number b):rest) = Right $ (Number $ b / a):rest
applyOperator (Operator x) _ = Left $ "Error, cannot apply binary operator " ++ [x] ++ "."
applyOperator _ _ = error "This shouldn't appear? ;-;"