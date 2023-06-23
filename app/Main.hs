module Main
  ( main,
  )
where

import HsBlog
import OptParse
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output replace env -> do
      exists <- doesDirectoryExist output
      shouldOpenFile <- if exists && not replace then confirm else pure True
      if shouldOpenFile then convertDirectory env input output else exitFailure
    ConvertSingle input output replace env -> do
      (title, inputHandle) <- case input of
        Stdin -> pure ("", stdin)
        InputFile file -> (,) file <$> openFile file ReadMode
      outputHandle <- case output of
        Stdout -> pure stdout
        OutputFile file -> do
          exists <- doesFileExist file
          shouldOpenFile <- if exists && not replace then confirm else pure True
          if shouldOpenFile then openFile file WriteMode else exitFailure
      convertSingle env title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

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
