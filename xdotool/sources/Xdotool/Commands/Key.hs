--------------------------------------------------
--------------------------------------------------

{-|

@
xdotool key [<option> ...] <keysym>
@

-}

module Xdotool.Commands.Key where

--------------------------------------------------

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
X.key X.def "alt-Tab"
@

-}

key :: (MonadXdotool m) => KeyOptions Maybe -> KeySymbol -> m ()
key options = \(KeySymbol t) -> do
  
  xdotool_ "key" os [t]

  where
  os = concat (renderKeyOptions options)

----

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-|

-}

newtype KeySymbol = KeySymbol

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

instance IsString KeySymbol where
  fromString = coerce

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