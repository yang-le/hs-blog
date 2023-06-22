module HsBlog
  ( convertSingle,
    convertDirectory,
  )
where

import HsBlog.Convert (convert)
import HsBlog.Directory
import HsBlog.Html
import HsBlog.Markup
import System.IO

convertSingle :: Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Title -> String -> String
process title = render . convert title . parse
