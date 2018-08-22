--------------------------------------------------
--------------------------------------------------

{-|



-}

module Xdotool.Shell where

-- XDoTool.Shell
-- Xdotool.Shell

--------------------------------------------------

import "base"    System.Exit
import "process" System.Process

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

data ShellException = ShellException

  { exitCode      :: Word8  -- ^ @2^8 = 256@; exit codes are in the range @[0..255]@; an overflowing argument to a shell @exit@ is modulo-255 anyways.
  , standardError :: String -- ^ @stderr@, if any.
  }
  
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Exception)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| 

-}

executeIO :: String -> [String] -> IO String
executeIO = executeM 

--------------------------------------------------

{-| 

-}

executeM
  :: (MonadIO m, MonadThrow m)
  => String -> [String] -> m String

executeM executable arguments = 
  executeE executable arguments >>= throwEither

--------------------------------------------------

{-| 

-}

executeE
  :: (MonadIO m)
  => String -> [String] -> m (Either ShellException String)

executeE executable arguments = do

  io (readProcessWithExitCode executable arguments "") <&> go

  where
  go = \case    
   (ExitSuccess,   stdout,             _) -> Right $ stdout
   (ExitFailure i,      _, standardError) -> Left  $ ShellException{..} where exitCode = fromIntegral i

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------