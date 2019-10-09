module Main where
    
import Stack
import Values
import Stackulator
import System.IO

main :: IO ()
main = env []

env :: [Value] -> IO ()
env stack = do
    putStr $ showStack stack                               -- Display the stack
    putStr "\n> "                                       -- Display the prompt
    hFlush stdout
    (message, nstack) <- onInput stack <$> getLine      -- Run the onInput function on the stack and the given input
    if null message                               
        then return ()                                  -- If no message, ignore
        else putStrLn message                           -- If a message was returned, then print it.
    env nstack                                          -- Run this env function on the new stack.
    
onInput :: [Value] -> String -> (String, [Value])
onInput stack "" = ("You must input something!", stack)
onInput stack str = 
    let value = fromInput str
    in
        case value of
            Nothing -> ("Invalid input", stack)
            Just Pop -> ("", popStack stack)
            Just Evaluate -> ("", reduceStack stack)
            Just anything_else -> ("", pushStack anything_else stack)