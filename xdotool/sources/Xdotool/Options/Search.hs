{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}

--------------------------------------------------
--------------------------------------------------

{-|



@
WINDOW COMMANDS

       search [options] pattern

           Search for windows with titles, names, or classes with a regular expression pattern.
           The output is line-delimited list of X window identifiers. If you are using "COMMAND
           CHAINING", the search command will only write window ids to stdout if it is the last
           (or only) command in the chain; otherwise, it is silent.

           The result is saved to the window stack for future chained commands. See "WINDOW
           STACK" and "COMMAND CHAINING" for details.

           The default options are "--name --class --classname" (unless you specify one one or
           more of --name --class or --classname).

@

-}

module Xdotool.Options.Search where

--------------------------------------------------

import Xdotool.Options.Global

--------------------------------------------------

import qualified "lens"         Control.Lens          as L
import qualified "generic-lens" Data.Generics.Product as L

--------------------------------------------------

import qualified "containers" Data.Set as Set

--------------------------------------------------

import "base" Text.Show
import "base" Data.Functor.Classes

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-|

this type's fields correspond (approximately) to command-specific options for @xdotool search ...@,
as well as the global options shared among all @xdotool@ commands.

* 'pid'
          @
           --pid PID
               Match windows that belong to a specific process id. This may not work for some X
               applications that do not set this metadata on its windows.
          @


* 'connective'
          @
           --any
               Match windows that match any condition (logically, 'or'). This is on by default.
               For example:

                 $ xdotool search --any --pid 1424 --name "Hello World"

               This will match any windows owned by pid 1424 or windows with name "Hello World"

           --all

               Require that all conditions be met. For example:

                 $ xdotool search --all --pid 1424 --name "Hello World"

               This will match only windows that have "Hello World" as a name and are owned by
               pid 1424.
          @

* 'sync'
          @
           --sync
               Block until there are results. This is useful when you are launching an
               application want want to wait until the application window is visible.  For
               example:

                 $ firefox &
                 $ xdotool search --sync --onlyvisible --class "firefox"
          @

* 'onlyvisible'
          @
           --onlyvisible
               Show only visible windows in the results.
               This means ones with map state IsViewable.
          @

* 'screen'
          @
           --screen N
               Select windows only on a specific screen. Default is to search all screens. Only
               meaningful if you have multiple displays and are not using Xinerama.
          @

* 'desktop'
          @
           --desktop N
               Only match windows on a certain desktop. 'N' is a number. The default is to search
               all desktops.
          @

* 'maxdepth
          @'
           --maxdepth N
               Set recursion/child search depth. Default is -1, meaning infinite. 0 means no
               depth, only root windows will be searched. If you only want toplevel windows, set
               maxdepth of 1 (or 2, depending on how your window manager does decorations).
          @

* 'limit'
          @
           --limit N
               Stop searching after finding N matching windows. Specifying a limit will help
               speed up your search if you only want a few results.

               The default is no search limit (which is equivalent to \'--limit 0\')
          @

Also see:

* 'GlobalOptions'

-}

data SearchOptions f = SearchOptions

  -- global options
  { window         :: f WindowID
  , delay          :: f Milliseconds
  , clearmodifiers :: f ShouldClearModifiers
  , sync           :: f WhetherSynchronous

  -- search options
  , connective     :: f LogicalConnective
  , maxdepth       :: f MaxSearchDepth
  , limit          :: f SearchLimit
  , screen         :: f WhichScreenToSearch
  , desktop        :: f WhichDesktopToSearch

  -- window options
  , onlyvisible    :: f Visibility
  , pid            :: f ProcessID
  , properties     :: f WhichWindowProperties
  }
  
  deriving stock    (Generic)

  -- deriving stock    (Show,Read,Eq,Ord,Lift)
  -- deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultSearchOptions'@
instance Default (SearchOptions Maybe) where
  def = defaultSearchOptions

--------------------------------------------------

-- | 
instance (Show1 f) => Show (SearchOptions f) where

  showsPrec :: Int -> (SearchOptions f) -> (String -> String)
  -- showsPrec :: Int -> a -> ShowS
  showsPrec precedence SearchOptions{..} =

    showAlphanumericConstructor precedence
        [ showString "SearchOptions "
        , showString "{ window = ",         showsSubPrec1 window
        , showString ", delay = ",          showsSubPrec1 delay
        , showString ", clearmodifiers = ", showsSubPrec1 clearmodifiers
        , showString ", sync = ",           showsSubPrec1 sync
        , showString ", screen = ",         showsSubPrec1 screen
        , showString ", desktop = ",        showsSubPrec1 desktop
        , showString ", connective = ",     showsSubPrec1 connective
        , showString ", maxdepth = ",       showsSubPrec1 maxdepth
        , showString ", limit = ",          showsSubPrec1 limit
        , showString ", onlyvisible = ",    showsSubPrec1 onlyvisible
        , showString ", pid = ",            showsSubPrec1 pid
        , showString ", properties = ",     showsSubPrec1 properties
        , showString " }"
        ]

  --NOTE--
  --
  -- `precedence :: Int`
  -- the operator precedence of the enclosing context (a number from 0 to 11). Function application has precedence 10.
  --
  -- `showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS`
  --
  -- e.g.
  -- showsPrec d (Leaf m) =
  --   showParen (d > applicationPrecedence) $
  --     showString "Leaf " . showsPrec (applicationPrecedence + 1) m
  --       where applicationPrecedence = 10
  --
  -- `showString :: String -> ShowS`
  -- 
  -- 

--------------------------------------------------
--------------------------------------------------

{-|

Represents no options passed:

* 'window'         = 'Nothing': by default, @xdotool@ sends events to the current window.
* 'delay'          = 'Nothing': by default, @xdotool@ interperses a @12ms@ delay between each event.
* 'clearmodifiers' = 'Nothing': by default, @xdotool@ does *not* clear modifiers.
* 'sync'           = 'Nothing': by default, @xdotool@ ...
* 'screen'         = 'Nothing': by default, @xdotool@ ...
* 'desktop'        = 'Nothing': by default, @xdotool@ ...
* 'connective'     = 'Nothing': by default, @xdotool@ ...
* 'maxdepth'       = 'Nothing': by default, @xdotool@ ...
* 'limit'          = 'Nothing': by default, @xdotool@ ...
* 'onlyvisible'    = 'Nothing': by default, @xdotool@ ...
* 'pid'            = 'Nothing': by default, @xdotool@ ...
* 'properties'     = 'Nothing': by default, @xdotool@ ...

-}

defaultSearchOptions :: SearchOptions Maybe
defaultSearchOptions = SearchOptions{..}
  where

  window         = Nothing
  delay          = Nothing
  clearmodifiers = Nothing

  sync           = Nothing
  screen         = Nothing
  desktop        = Nothing
  connective     = Nothing
  maxdepth       = Nothing
  limit          = Nothing
  onlyvisible    = Nothing
  pid            = Nothing
  properties     = Nothing

--------------------------------------------------

{-|


-}

newtype WhichWindowProperties = WhichWindowProperties

  [WindowProperty]

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

-- | 'fromList' dedups and sorts.
instance IsList WhichWindowProperties where
  type Item WhichWindowProperties = WindowProperty
  fromList = coerce > normalizeWindowProperties
  toList   = coerce

--------------------------------------------------

normalizeWindowProperties :: WhichWindowProperties -> WhichWindowProperties
normalizeWindowProperties (WhichWindowProperties props)
  = WhichWindowProperties (go props)
  where
  go = Set.fromList > Set.toList > sort

--------------------------------------------------

{-|

Corresponding @xdotool@ options:

* 'WindowClass'
          @
           --class
               Match against the window class.
          @

* 'WindowClassName'
          @
           --classname
               Match against the window classname.
          @

* 'WindowName'
          @
           --name
               Match against the window name.
               This is the same string that is displayed in the window titlebar.
          @

-}

data WindowProperty

  = WindowClass
  | WindowName
  | WindowClassName

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  --deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

data WhichScreenToSearch
  = SearchAllScreens
  | SearchScreen     ScreenID

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Num)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

data WhichDesktopToSearch
  = SearchAllDesktops
  | SearchDesktop     DesktopID

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Num)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| a process identifier.

-}

newtype ProcessID = ProcessID

  Int

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Num)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

{-| an X-screen identifier.

-}

newtype ScreenID = ScreenID

  Int

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Num)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

{-| an X-desktop identifier.

-}

newtype DesktopID = DesktopID

  Int

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Num)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

{-|

@
'UnboundedSearchDepth' ≡ 'BoundedSearchDepth' -1
@

-}

data MaxSearchDepth
  = UnboundedSearchDepth
  | BoundedSearchDepth Int

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | 
pattern OnlySearchRootWindows :: MaxSearchDepth
pattern OnlySearchRootWindows = BoundedSearchDepth 0

-- | 
pattern OnlySearchTopLevelWindows1 :: MaxSearchDepth
pattern OnlySearchTopLevelWindows1 = BoundedSearchDepth 1

-- | 
pattern OnlySearchTopLevelWindows2 :: MaxSearchDepth
pattern OnlySearchTopLevelWindows2 = BoundedSearchDepth 2

--------------------------------------------------

{-|

@
'UnlimitedSearch' ≡ 'LimitedSearch' 0
@

-}

data SearchLimit
  = UnlimitedSearch
  | LimitedSearch Int

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

Naming: logical "connectives" include conjunction and disjunction.

-}

data LogicalConnective
  = AnyCondition
  | AllConditions

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

data WhetherSynchronous

  = Synchronous
  | Asynchronous

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|

-}

data Visibility

  = OnlyVisible
  | AlsoInvisible

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  -- deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

e.g.

@
$ xprop
...
WM_CLASS(STRING) = "_emacs-wrapped", "Emacs"
WM_NAME(STRING) = "Emacs — Xdotool.hs"

$ make repl

Xdotool> :set -XOverloadedLists

Xdotool> let os = renderSearchOptions SearchOptions{ properties = Prelude.Just [ WindowClass, WindowClassName ], window = Prelude.Just 94371868, delay = Prelude.Just 100, clearmodifiers = Prelude.Just ClearModifiers, sync = Prelude.Just Synchronous, connective = Prelude.Just AllConditions, pid = Prelude.Just 998, screen = Prelude.Just (SearchScreen 2), desktop = Prelude.Just (SearchDesktop 5), maxdepth = Prelude.Just OnlySearchTopLevelWindows2, limit = Prelude.Just (LimitedSearch 10), onlyvisible = Prelude.Just OnlyVisible }

Xdotool> Prelude.putStrLn (Data.List.intercalate " " (Data.List.concat os))
--all --class --classname --clearmodifiers --delay 100 --desktop 5 --limit 10 --maxdepth 2 --onlyvisible --pid 998 --screen 2 --sync --window 94371868

Xdotool> Prelude.putStrLn `Data.Foldable.traverse_` (Data.List.intercalate " " `Prelude.fmap` os)
--all
--class --classname
--clearmodifiers
--delay 100
--desktop 5
--limit 10
--maxdepth 2
--onlyvisible
--pid 998
--screen 2
--sync
--window 94371868

@

@doctest@s:

>>> :set -XOverloadedLists
>>> renderSearchOptions SearchOptions{ properties = Prelude.Just [ WindowClass, WindowClassName ], window = Prelude.Just 94371868, delay = Prelude.Just 100, clearmodifiers = Prelude.Just ClearModifiers, sync = Prelude.Just Synchronous, connective = Prelude.Just AllConditions, pid = Prelude.Just 998, screen = Prelude.Just (SearchScreen 2), desktop = Prelude.Just (SearchDesktop 5), maxdepth = Prelude.Just OnlySearchTopLevelWindows2, limit = Prelude.Just (LimitedSearch 10), onlyvisible = Prelude.Just OnlyVisible }
[["--all"],["--class","--classname"],["--clearmodifiers"],["--delay","100"],["--desktop","5"],["--limit","10"],["--maxdepth","2"],["--onlyvisible"],["--pid","998"],["--screen","2"],["--sync"],["--window","94371868"]]

-}

renderSearchOptions :: SearchOptions Maybe -> [[String]]
renderSearchOptions options = go searchOptions

  where
  go = filter (/= []) > sort
  
  searchOptions = globalOptions ++ searchOnlyOptions

  globalOptions
    = renderGlobalOptions (options L.^. L.super @(GlobalOptions Maybe))

  searchOnlyOptions
    = options
    & renderSearchOnlyOptions

  --notes--
  --   & L.super @GlobalOptions L.%~ renderGlobalOptions

--------------------------------------------------

{-|

-}

renderSearchOnlyOptions
 :: SearchOptions Maybe -> [[String]]
renderSearchOnlyOptions SearchOptions{..} =
  
  [ sync         & maybe [] (\case
      Synchronous                 -> ["--sync"]
      Asynchronous                -> []
                            )

  , connective   & maybe [] (\case
      AnyCondition                -> ["--any"]
      AllConditions               -> ["--all"]
                            )

  , pid          & maybe [] (\case
      ProcessID i                 -> [ "--pid", show i ]
                            )

  , screen       & maybe [] (\case
      SearchAllScreens            -> []
      SearchScreen (ScreenID i)   -> [ "--screen", show i ]
                            )

  , desktop      & maybe [] (\case
      SearchAllDesktops           -> []
      SearchDesktop (DesktopID i) -> [ "--desktop", show i ]
                            )

  , maxdepth     & maybe [] (\case
      UnboundedSearchDepth        -> [ "--maxdepth",   "-1" ]
      BoundedSearchDepth i        -> [ "--maxdepth", show i ]
                            )

  , limit        & maybe [] (\case
      UnlimitedSearch             -> [ "--limit",    "0" ]
      LimitedSearch i             -> [ "--limit", show i ]
                            )

  , onlyvisible  & maybe [] (\case
      OnlyVisible                 -> ["--onlyvisible"]
      AlsoInvisible               -> []
                            )

  , properties   & maybe [] renderWindowPropertiesOptions
  
  ]

--------------------------------------------------

-- |
--

renderWindowPropertiesOptions :: WhichWindowProperties -> [String]
renderWindowPropertiesOptions = normalizeWindowProperties > go
  where
  go (WhichWindowProperties props) =
    renderWindowPropertyOption <$> props

-- |
--

renderWindowPropertyOption :: WindowProperty -> String
renderWindowPropertyOption = \case
  WindowClass     -> "--class"
  WindowName      -> "--name"
  WindowClassName -> "--classname"

--------------------------------------------------

-- |
--

renderWMClassOption :: String -> [String]
renderWMClassOption = \case
  "" -> []
  t  -> ["--class", t]

--------------------------------------------------

-- |
--

renderWMClassNameOption :: String -> [String]
renderWMClassNameOption = \case
  "" -> []
  t  -> ["--classname", t]

--------------------------------------------------
{- Notes -----------------------------------------

--------------------------------------------------

--------------------------------------------------

-------------------------------------------------}