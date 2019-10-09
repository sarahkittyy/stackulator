-- | Defines a stack and it's operations.
module Stack where

-- | Alias for a stack.
newtype Stack a = Stack { getStack :: [a] }

-- | Push an element to the top of the stack
pushStack :: a -> Stack a -> Stack a
pushStack val stack = Stack $ (getStack stack) ++ [val]

-- | Get the element at the top of the stack
topStack :: Stack a -> a
topStack = last . getStack

-- | Pop the element at the top of the stack
popStack :: Stack a -> Stack a
popStack a = Stack $ init . getStack $ a

-- | The max width of the stack display
stackWidth :: Int
stackWidth = 7

-- | For displaying a stack.
instance (Show a) => Show (Stack a) where
    show (Stack []) = replicate stackWidth '-'
    show (Stack (x:xs)) = str ++ show (Stack xs)
        where
            len = length $ show x
            padding = if stackWidth - len <= 0
                        then 0
                        else (stackWidth - len) `div` 2
            str = (replicate padding ' ') ++ (show x) ++ "\n"