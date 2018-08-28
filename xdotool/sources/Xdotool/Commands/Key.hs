--------------------------------------------------
--------------------------------------------------

{-|

@
xdotool key [<option> ...] <keysym>
@

-}

module Xdotool.Commands.Key where

--------------------------------------------------

import Xdotool.KeySymbol
import Xdotool.Core
import Xdotool.Options.Key

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-| 

e.g.

@
import qualified Xdotool as X

:set -XOverloadedStrings
X.key X.def ["alt+Tab"]

:set -XDuplicateRecordFields
X.key X.def{ delay = 1000, clearmodifiers = Just ClearModifiers } ["ctrl+a", "alt+f"]
@

should be equivalent to:

@
$ xdotool key --delay 1000 'ctrl+a' 'alt+f'
@

helm-boring-buffer-regexp-list

-}

key :: (MonadXdotool m) => KeyOptions Maybe -> [KeySymbol] -> m ()
key options = \keysyms -> do
  
  let args = coerce <$> keysyms
  
  xdotool_ "key" opts args

  where
  opts = concat (renderKeyOptions options)

----

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

{-| 

-}

--------------------------------------------------
--------------------------------------------------