module Stackulator
( reduceStack
) where
    
import Values
import Stack

reduceStack :: [Value] -> [Value]
reduceStack [] = []
reduceStack [a] = [a]
reduceStack [a,b] = [a,b]
reduceStack ((Number n):rest) = (Number n):rest
reduceStack ((Operator x):(Number a):(Number b):rest)
    | x == '+' = reduceStack $ pushStack (Number $ a + b) rest
    | x == '-' =reduceStack $ pushStack (Number $ a - b) rest
    | x == '*' = reduceStack $ pushStack (Number $ a * b) rest
    | x == '/' = reduceStack $ pushStack (Number $ a / b) rest