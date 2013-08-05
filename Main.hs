module Main (main) where

import Control.Monad (unless)
import Data.List (last, take, length)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import PureJson (parseJson)

getInput = do
    line <- getLine
    case last line of {
        ';' -> do {
            let len = length line;
                size = len - 1;
            in return (take size line); };
        _ -> do {
            trail <- getInput;
            return (line ++ trail); }; }
repl = do
    t <- getInput
    let j = parseJson t;
    print j
    repl

main = do
    putStrLn ("Pure JSON 0.5.1")
    repl