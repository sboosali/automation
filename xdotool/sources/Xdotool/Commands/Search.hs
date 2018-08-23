--------------------------------------------------
--------------------------------------------------

{-|



-}

module Xdotool.Commands.Search where

--------------------------------------------------

import Xdotool.Core
import Xdotool.Options.Search

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-|

e.g.

@
Xdotool> runningEmacsWindows <- 'search' 'defaultSearchOptions' { query = 'queryClass' "Emacs" }
Xdotool> Prelude.putStrLn runningEmacsWindows 
94371841
94753167
94753188
94372902
94371910
94371868

Xdotool> visibleEmacsWindows <- search defaultSearchOptions { query = queryClass "Emacs", onlyvisible = Prelude.Just 'OnlyVisible' }
Xdotool> Prelude.putStrLn visibleEmacsWindows
94371868

@

-}

search :: (MonadXdotool m) => SearchOptions Maybe -> m String
search options = do
  
  xdotool "search" os []

  where
  os = concat (renderSearchOptions options)

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