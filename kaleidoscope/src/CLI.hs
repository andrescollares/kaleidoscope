module CLI where
import Options.Applicative
import System.Console.Haskeline (InputT, getInputLine)

data CliOptions = CliOptions
  { optimizationLevel      :: Word
  , inputFile      :: String
  , emitLLVM      :: Bool
  , failOnErrors :: Bool
  }

options :: Parser CliOptions
options = CliOptions
      <$> option auto
          ( long "opt-level"
         <> short 'o'
         <> metavar "OPT"
         <> value 3
         <> help "Optimization level 0-3" )
      <*> strOption
          ( long "file"
         <> short 'f'
         <> metavar "FILE"
         <> value []
         <> help "File to read from" )
      <*> flag True False
          ( long "quiet-llvm"
         <> short 'q'
         <> help "Hide LLVM IR output" )
      <*> flag False True
          ( long "fail-on-errors"
          <> short 'e'
          <> help "Fail on errors" )

opts :: ParserInfo CliOptions
opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Kaleidoscope compiler"
     <> header "Kaleidoscope compiler" )

lastCharOrEmpty :: String -> Char
lastCharOrEmpty [] = ' '
lastCharOrEmpty s = last s

getNextInput :: InputT IO (Maybe String)
getNextInput = do
  nextInputLine <- getInputLine ""
  case nextInputLine of
    Just line -> case lastCharOrEmpty line of
      ';' -> return $ Just line
      _ -> do
        nextLine <- getNextInput
        case nextLine of
          Nothing -> return $ Just line
          Just next -> return $ Just $ line ++ ' ' : next
    Nothing -> return Nothing