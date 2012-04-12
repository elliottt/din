module Options where

import Din.Types (LogLevel(..))

import System.Console.GetOpt
    (ArgOrder(..),OptDescr(..),ArgDescr(..),usageInfo,getOpt)
import System.Directory (getHomeDirectory)
import System.Environment (getProgName)
import System.Exit (exitFailure,exitSuccess)
import System.FilePath ((</>),takeDirectory)


-- Options ---------------------------------------------------------------------

-- | Command-line options.
data Options = Options
  { optLogLevel   :: LogLevel
  , optConfigFile :: FilePath
  } deriving (Show)

-- | Default @Options@.
defaultOptions :: IO Options
defaultOptions  = do
  home <- getHomeDirectory
  return Options
    { optLogLevel   = LogError
    , optConfigFile = home </> ".din" </> "config"
    }


-- Options Parsing -------------------------------------------------------------

-- | Parsers for individual flags.
type Parse = IO (Options -> Options)

options :: [OptDescr Parse]
options  =
  [ Option "h" ["help"] (NoArg displayHelp)
    "Display this message"

  , Option "c" ["config"] (ReqArg setConfigFile "PATH")
    "Location of the config file"

  , Option "v" ["log-level"] (ReqArg parseLogLevel "INT")
    "Log level for console output"
  ]

-- | Handler for the -h switch.
displayHelp :: Parse
displayHelp  = do
  displayUsage []
  exitSuccess

-- | Setters for required option arguments.
type Required = String -> Parse

-- | Set the optConfigFile field of an @Options@.
setConfigFile :: Required
setConfigFile str = return (\opts -> opts { optConfigFile = str })

-- | Parse out the optLogLevel field of an @Options@ record.
parseLogLevel :: Required
parseLogLevel str = case reads str of
  [(n,[])] -> setLogLevel n
  _        -> fail ("Invalid log level: " ++ str)

-- | Set the log level in an @Options@ structure.
setLogLevel :: Int -> Parse
setLogLevel n
  | validLogLevel n = return (\opts -> opts { optLogLevel = toEnum n })
  | otherwise       = fail "Invalid log level, must be in the range [0,3]"

-- | Deterimine if an integer representation of a @LogLevel@ is valid.
validLogLevel :: Int -> Bool
validLogLevel n = n >= fromEnum LogError && n <= fromEnum LogDebug

-- | Parse an @Options@ from a set of input arguments.
parseOptions :: [String] -> IO Options
parseOptions args = do
  case getOpt Permute options args of

    (fs,_,[])  -> do
      setters  <- sequence fs
      foldl (.) id setters `fmap` defaultOptions

    (_,_,errs) -> do
      displayUsage errs
      exitFailure

-- | Display the usage string for the options.
displayUsage :: [String] -> IO ()
displayUsage errs = do
  prog <- getProgName
  let banner = unlines (errs ++ ["usage: " ++ prog ++ " [OPTIONS]"])
  putStrLn (usageInfo banner options)
