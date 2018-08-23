--------------------------------------------------
--------------------------------------------------

{-|

-}

module Xdotool.Commands.Type where

--------------------------------------------------

import Xdotool.Core
import Xdotool.Options.Global

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-|

e.g.

@
insert' defaultGlobalOptions "$hello, `world`"
@


Naming: @insert@ some text (@type@ is a Haskell keyword).

-}

insert' :: (MonadXdotool m) => GlobalOptions Maybe -> String -> m ()
insert' options = \t -> do
  _ <- xdotool' "type" options' [t]
  nothing

  where
  options' = renderGlobalOptions options

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