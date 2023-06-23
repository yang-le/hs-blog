module HsBlog.Directory
  ( convertDirectory,
    buildIndex,
  )
where

import Control.Exception (SomeException (..), catch, displayException)
import Control.Monad (void, when)
import Data.List (partition)
import Data.Traversable (for)
import HsBlog.Convert (convert, convertStructure)
import HsBlog.Env (Env (eBlogName, eStylesheetPath))
import HsBlog.Html
import HsBlog.Markup
import System.Directory
  ( copyFile,
    createDirectory,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath
  ( takeBaseName,
    takeExtension,
    takeFileName,
    (<.>),
    (</>),
  )
import System.IO (hPutStrLn, stderr)

data DirContents = DirContents
  { dcFilesToProcess :: [(FilePath, String)],
    dcFilesToCopy :: [FilePath]
  }

buildIndex :: Env -> [(FilePath, Document)] -> Html
buildIndex env list =
  html_
    (title_ (eBlogName env) <> stylesheet_ (eStylesheetPath env))
    ( h_ 1 (link_ "index.html" (txt_ "Blog"))
        <> h_ 2 (txt_ "Posts")
        <> foldMap buildIndexEntry list
    )

buildIndexEntry :: (FilePath, Document) -> HsBlog.Html.Structure
buildIndexEntry (path, doc) = case doc of
  Heading 1 t : article ->
    h_ 3 (link_ path (txt_ t))
      <> foldMap convertStructure (take 3 article)
      <> p_ (link_ path (txt_ "..."))
  _ -> h_ 3 (link_ path (txt_ path))

convertDirectory :: Env -> FilePath -> FilePath -> IO ()
convertDirectory env inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectory outputDir
  let outputHtmls = txtsToRenderedHtml env filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (txtFiles, otherFiles) = partition ((== ".txt") . takeExtension) files
  txtFilesAndContent' <- applyIoOnList readFile txtFiles
  txtFilesAndContent <- filterAndReportFailures txtFilesAndContent'
  pure $
    DirContents
      { dcFilesToProcess = txtFilesAndContent,
        dcFilesToCopy = otherFiles
      }

-- applyIo :: (a -> IO b) -> a -> IO (a, Either String b)
-- applyIo io item =
--   catch
--     ( do
--         result <- io item
--         pure (item, Right result)
--     )
--     ( \(SomeException e) ->
--         pure (item, Left (displayException e))
--     )

-- applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
-- applyIoOnList io = traverse (applyIo io)

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action list =
  for list $ \item -> do
    result <-
      catch
        (Right <$> action item)
        ( \(SomeException e) ->
            pure (Left (displayException e))
        )
    pure (item, result)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(first, second) -> case second of
    Left err -> do
      hPutStrLn stderr err
      pure []
    Right result -> pure [(first, result)]

createOutputDirectory :: FilePath -> IO ()
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  when dirExists (removeDirectoryRecursive dir)
  createDirectory dir

txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml env input = index : documentToRenderedHtml env docs
  where
    index = ("index.html", render (buildIndex env docs))
    docs = txtsToDocument input

txtsToDocument :: [(FilePath, String)] -> [(FilePath, Document)]
txtsToDocument = map $ \(file, content) ->
  (takeBaseName file <.> "html", parse content)

documentToRenderedHtml :: Env -> [(FilePath, Document)] -> [(FilePath, String)]
documentToRenderedHtml env = map $ \(file, doc) ->
  (file, render (convert env file doc))

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  files' <- applyIoOnList copyFromTo files
  void $ filterAndReportFailures files'
  where
    copyFromTo file = copyFile file (outputDir </> takeFileName file)

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  files' <- applyIoOnList writeFileContent files
  void $ filterAndReportFailures files'
  where
    writeFileContent (file, content) = writeFile (outputDir </> file) content
