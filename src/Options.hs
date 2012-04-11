module Options where

import Din.Types (LogLevel(..),Env(..))
import Storage.Db (withStorage)

import System.Console.GetOpt
    (ArgOrder(..),OptDescr(..),ArgDescr(..),usageInfo,getOpt)
import System.Directory (getHomeDirectory)
import System.Environment (getProgName)
import System.Exit (exitFailure,exitSuccess)
import System.FilePath ((</>))

-- Options ---------------------------------------------------------------------

-- | Command-line options.
data Options = Options
  { optLogLevel   :: LogLevel
  , optDbLocation :: FilePath
  } deriving (Show)

-- | Default @Options@.
defaultOptions :: IO Options
defaultOptions  = do
  home <- getHomeDirectory
  return Options
    { optLogLevel   = LogError
    , optDbLocation = home </> ".din" </> "library.db"
    }

-- | Run a continuation with the environment generated by an @Options@.
withOptionsEnv :: Options -> (Env -> IO a) -> IO a
withOptionsEnv opts k = withStorage (optDbLocation opts) body
  where
  body h = k Env
    { envDbHandle = h
    , envLogLevel = optLogLevel opts
    }


-- Options Parsing -------------------------------------------------------------

type Parse = IO (Options -> Options)

options :: [OptDescr Parse]
options  =
  [ Option "h" ["help"] (NoArg displayHelp)
    "Display this message"

  , Option "d" ["database"] (ReqArg setDbLocation "PATH")
    "Location of the song database"

  , Option "l" ["log-level"] (ReqArg parseLogLevel "INT")
    "Log level for console output"
  ]

displayHelp :: Parse
displayHelp  = do
  displayUsage []
  exitSuccess

type Required = String -> Parse

setDbLocation :: Required
setDbLocation str = return (\opts -> opts { optDbLocation = str })

parseLogLevel :: Required
parseLogLevel str = case reads str of
  [(n,[])] -> setLogLevel n
  _        -> fail ("Invalid log level: " ++ str)

setLogLevel :: Int -> Parse
setLogLevel n
  | validLogLevel n = return (\opts -> opts { optLogLevel = toEnum n })
  | otherwise       = fail "Invalid log level, must be in the range [0,3]"

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

displayUsage :: [String] -> IO ()
displayUsage errs = do
  prog <- getProgName
  let banner = unlines (errs ++ ["usage: " ++ prog ++ " [OPTIONS]\n"])
  putStrLn (usageInfo banner options)
