{-# LANGUAGE ConstraintKinds #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Xdotool.Core where

--------------------------------------------------

import Xdotool.Shell

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-| a @constraint@ alias.

-}

type MonadXdotool m = (MonadIO m, MonadThrow m)

--------------------------------------------------

{-| an untyped call to the @xdotool@ program.

all strings are properly escaped before getting passed to the @shell@.

Inputs:

@
xdotool' command options arguments
@

Examples:

this expression:

@
> xdotool' "type" ["--clearmodifiers", "--delay", "100"] ["$hello, ", "*world*"]
@

runs (the equivalent of) this script:

@
$ xdotool type --clearmodifiers --delay 100 '$hello, ' '*world*'
@

-}

xdotool :: (MonadXdotool m) => String -> [String] -> [String] -> m String
xdotool command options arguments = do
  executeM "xdotool" ([command] ++ options ++ arguments)

--------------------------------------------------

{-| 'xdotool' with @stdout@ ignored.

-}

xdotool_ :: (MonadXdotool m) => String -> [String] -> [String] -> m ()
xdotool_ command options arguments = do
  _ <- xdotool command options arguments
  nothing

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------