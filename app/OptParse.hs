module OptParse
  ( Options (..),
    SingleInput (..),
    SingleOutput (..),
    parse,
  )
where

import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput Bool
  | ConvertDir FilePath FilePath Bool
  deriving (Show)

data SingleInput = Stdin | InputFile FilePath deriving (Show)

data SingleOutput = Stdout | OutputFile FilePath deriving (Show)

pInputFile :: Parser SingleInput
pInputFile = InputFile <$> parser
  where
    parser =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input file"
        )

pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file"
        )

pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

pReplaceFile :: Parser Bool
pReplaceFile =
  switch
    ( long "replace"
        <> short 'r'
        <> help "Replace exists output file"
    )

pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput <*> pReplaceFile

pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
        <> short 'i'
        <> metavar "DIRECTORY"
        <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "DIRECTORY"
        <> help "Output directory"
    )

pReplaceDir :: Parser Bool
pReplaceDir =
  switch
    ( long "replace"
        <> short 'r'
        <> help "Replace exists output directory"
    )

pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir <*> pReplaceDir

pOptions :: Parser Options
pOptions =
  subparser
    ( command
        "convert"
        ( info
            (helper <*> pConvertSingle)
            (progDesc "Convert a single makrup source to html")
        )
        <> command
          "convert-dir"
          ( info
              (helper <*> pConvertDir)
              (progDesc "Convert a directory of markup files to html")
          )
    )

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
        <> header "hs-blog-gen - a static blog generator"
        <> progDesc "Convert markup files or directories to html"
    )

parse :: IO Options
parse = execParser opts
