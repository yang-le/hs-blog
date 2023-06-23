module HsBlog
  ( convertSingle,
    convertDirectory,
  )
where

import HsBlog.Convert (convert)
import HsBlog.Directory
import HsBlog.Env (Env)
import HsBlog.Html
import HsBlog.Markup
import System.IO

convertSingle :: Env -> String -> Handle -> Handle -> IO ()
convertSingle env title input output = do
  content <- hGetContents input
  hPutStrLn output (process env title content)

process :: Env -> String -> String -> String
process env title = render . convert env title . parse
