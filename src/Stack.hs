-- | Defines a stack and it's operations.
module Stack where

-- | Push an element to the top of the stack
pushStack :: a -> [a] -> [a]
pushStack val stack = val:stack

-- | Get the element at the top of the stack
topStack :: [a] -> a
topStack = last

-- | Pop the element at the top of the stack
popStack :: [a] -> [a]
popStack = init

-- | The max width of the stack display
stackWidth :: Int
stackWidth = 7

-- | For displaying a stack.
showStack :: (Show a) => [a] -> String
showStack [] = replicate stackWidth '-'
showStack (x:xs) = str ++ showStack xs
    where
        len = length $ show x
        padding = if stackWidth - len <= 0
                    then 0
                    else (stackWidth - len) `div` 2
        str = (replicate padding ' ') ++ (show x) ++ "\n"