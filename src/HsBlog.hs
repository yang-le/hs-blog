module HsBlog (main, process) where

import HsBlog.Html
import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import Control.Monad ( when )
import HsBlog.Markup ( parse )
import HsBlog.Convert ( convert )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            contents <- getContents
            putStr (process "Empty title" contents)
        [infile, outfile] -> do
            contents <- readFile infile
            exist <- doesFileExist outfile
            let writeResult = writeFile outfile (process outfile contents)
            if exist then whenIO confirm writeResult else writeResult
        _ -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Title -> String -> String
process title = render . convert title . parse

confirm :: IO Bool
confirm = do
    putStrLn "Are you sure? (y/n)"
    answer <- getLine
    case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> do
            putStrLn "Invalid response. use y or n"
            confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
    result <- cond
    when result action
