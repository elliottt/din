{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative (many,(<|>))
import Control.Monad (void)
import Data.Attoparsec.Combinator (manyTill)
import Data.Attoparsec.Text
    (Parser,string,skipWhile,(<?>),skip,char,endOfLine,anyChar,endOfInput
    ,skipWhile)
import Data.Attoparsec.Text.Lazy (eitherResult,parse)
import Data.Char (ord,isSpace)
import System.FilePath ((</>))
import qualified Data.ByteString as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L


data Config = Config
  { cfgWatchPaths :: [FilePath]
  , cfgDbPath     :: FilePath
  } deriving (Show)

emptyConfig :: FilePath -> Config
emptyConfig dir = Config
  { cfgWatchPaths = []
  , cfgDbPath     = dir </> "library.db"
  }

type Directive = Config -> Config

addWatchPath :: String -> Directive
addWatchPath path cfg = cfg { cfgWatchPaths = cfgWatchPaths cfg ++ [path] }

setDbPath :: String -> Directive
setDbPath path cfg = cfg { cfgDbPath = path }

-- | Parse a config file.
parseConfig :: FilePath -> L.Text -> Either String Config
parseConfig dir = eitherResult . parse (configFile dir)

-- | Parser for a config file.
configFile :: FilePath -> Parser Config
configFile dir =
  foldl (flip id) (emptyConfig dir) `fmap` manyTill directive endOfInput

-- | Skip space characters.
spaces :: Parser ()
spaces  = skipWhile (`elem` " \t")

-- | Trim the spaces from the end of a string.
trimTail :: String -> String
trimTail  = reverse . dropWhile isSpace . reverse

-- | Parser for a single directive from the config file.
directive :: Parser Directive
directive  = command "watch"    addWatchPath
         <|> command "database" setDbPath
         <|> command "db"       setDbPath
         <|> (spaces >> endOfLine >> directive)
  where
  command c k = do
    string c
    spaces
    (k . trimTail) `fmap`  manyTill anyChar endOfLine
