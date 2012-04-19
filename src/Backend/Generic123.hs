module Backend.Generic123 (
    mpg321Backend
  , ogg123Backend
  ) where

import Backend

import Control.Concurrent
import Control.Monad
import System.IO
import System.Process
    (shell,CreateProcess(..),StdStream(..),createProcess,waitForProcess)

-- | A backend for controlling mpg321.
mpg321Backend :: IO Backend
mpg321Backend  = generic123Backend "mpg321" "mpg321 -R mp"

-- | A backend for controlling ogg123.
ogg123Backend :: IO Backend
ogg123Backend  = generic123Backend "ogg123" "ogg123 -R mp"

-- | A generic backend for audio programs that support an mpg123-like interface.
generic123Backend :: String -> String -> IO Backend
generic123Backend name cmd = do
  let process = (shell cmd)
        { std_in       = CreatePipe
        , std_out      = CreatePipe
        , std_err      = CreatePipe
        , close_fds    = True
        }
  (Just inH, Just outH, Just errH, ph) <- createProcess process

  hSetBuffering inH NoBuffering
  hSetBinaryMode inH  False
  hSetBinaryMode outH False
  hSetBinaryMode errH False

  status    <- newSampleVar ""
  statusTid <- forkIO $ forever $ do
    line <- hGetLine outH
    unless (null line) (writeSampleVar status line)

  let send ws = do
        putStrLn ("Sending: " ++ show ws)
        hPutStrLn inH (unwords ws)
      cleanup = do
        send ["QUIT"]
        killThread statusTid
        hClose inH
        hClose outH
        hClose errH
        _ <- waitForProcess ph
        return ()

  return Backend
    { backendName      = name
    , backendLoad      = \ path -> send ["LOAD",path]
    , backendPause     =           send ["PAUSE"]
    , backendPlay      =           send ["PAUSE"]
    , backendStop      =           send ["STOP"]
    , backendCleanup   = cleanup
    , backendStatus    = readSampleVar status
    , backendSetVolume = \ vol -> send ["GAIN",show vol]
    }
