module Main where

import Lib
import Data.List
import System.Environment

main :: IO ()
main = do
    (file:_) <- getArgs
    text <- readFile file
    putStr "["
    sequence_ (map putStr (intersperse ", " (map show (lexer text))))
    putStrLn "]"