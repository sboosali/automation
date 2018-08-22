--------------------------------------------------
--------------------------------------------------

{-|



-}

module Xdotool.Options.Global where

--------------------------------------------------

import "base" Text.Show
import "base" Data.Functor.Classes

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

Specializations:

* @(GlobalOptions 'Maybe')@: if a field is 'Nothing', don't pass its corresponding command-line option (i.e. use @xdotool@'s implicit default). See @$ man xdotool@.
* @(GlobalOptions ('Const' [String]))@: each field has been rendered into its corresponding command-line bash strings.

-}

data GlobalOptions f = GlobalOptions
  { window         :: f WindowID
  , delay          :: f Milliseconds
  , clearmodifiers :: f ShouldClearModifiers
  }
  deriving stock    (Generic)

instance (Show1 f) => Show (GlobalOptions f) where

  showsPrec :: Int -> (GlobalOptions f) -> (String -> String)
  -- showsPrec :: Int -> a -> ShowS
  showsPrec precedence GlobalOptions{..} =

    showParen (precedence >= subexpressionPrecedence)
      (foldShowS
        [ showString "GlobalOptions "
        , showString "{ window = ",         showsSubPrec1 window
        , showString ", delay = ",          showsSubPrec1 delay
        , showString ", clearmodifiers = ", showsSubPrec1 clearmodifiers
        , showString " }"
        ])

    where

    showsSubPrec1 :: forall a. (Show a) => f a -> ShowS
    showsSubPrec1 = showsPrec1 applicationPrecedence
    --
    -- applicationPrecedence
    -- → { window = (Const ["--window","94371868"]), ... }
    --
    -- subexpressionPrecedence
    -- → { window = Const ["--window","94371868"], ... }
    --

    foldShowS :: [ShowS] -> ShowS
    foldShowS = foldr (.) id

    subexpressionPrecedence :: Int
    subexpressionPrecedence = 1 + applicationPrecedence
    
    applicationPrecedence :: Int
    applicationPrecedence = 10

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

-- | @= 'defaultGlobalOptions'@
instance Default (GlobalOptions Maybe) where
  def = defaultGlobalOptions

--------------------------------------------------

{-|

Represents no options passed:

* 'window'         = 'Nothing': by default, @xdotool@ sends events to the current window.
* 'delay'          = 'Nothing': by default, @xdotool@ interperses a @12ms@ delay between each event.
* 'clearmodifiers' = 'Nothing': by default, @xdotool@ does *not* clear modifiers.

-}

defaultGlobalOptions :: GlobalOptions Maybe
defaultGlobalOptions = GlobalOptions{..}
  where
  window         = Nothing
  delay          = Nothing
  clearmodifiers = Nothing

--------------------------------------------------

{-|

-}

newtype WindowID = WindowID

  Int

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Num)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

{-|

-}

newtype Milliseconds = Milliseconds

  Int

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Num)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

-- | @= '(+)'@
instance Semigroup Milliseconds where (<>)   = (+)
-- | @= '0'@
instance Monoid    Milliseconds where mempty = 0

--------------------------------------------------

{-|

-}

data ShouldClearModifiers

  = ClearModifiers
  | InheritModifiers

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  --deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)

-- | @= 'defaultShouldClearModifiers'@
instance Default ShouldClearModifiers where
  def = defaultShouldClearModifiers

--------------------------------------------------

{-| @= 'ClearModifiers'@
-}

defaultShouldClearModifiers :: ShouldClearModifiers
defaultShouldClearModifiers = ClearModifiers

--------------------------------------------------

{-|

e.g.

@
> renderGlobalOptions ('GlobalOptions' { 'window' = Just 94371868, 'delay' = Just 100, 'clearmodifiers' = Just 'ClearModifiers' })
["--window","94371868","--delay","100","--clearmodifiers"]
@

@doctest@s (TODO):

>>> renderGlobalOptions defaultGlobalOptions
[]
>>> renderGlobalOptions (GlobalOptions { window = Just 94371868, delay = Just 100, clearmodifiers = Just ClearModifiers })
["--window","94371868","--delay","100","--clearmodifiers"]

-}

renderGlobalOptions :: GlobalOptions Maybe -> [String]
renderGlobalOptions = renderGlobalOptionsF > go
  where
  go :: GlobalOptions (Const [String]) -> [String]
  go GlobalOptions
     { window         = (Const sWindow)
     , delay          = (Const sDelay)
     , clearmodifiers = (Const sClearModifiers)
     }
     = sWindow ++ sDelay ++ sClearModifiers

--------------------------------------------------

{-|

e.g.

@
> renderGlobalOptionsF 'defaultGlobalOptions'
GlobalOptions { 'window' = 'Const' [], 'delay' = 'Const' [], 'clearmodifiers' = 'Const' [] }
@

e.g.

@
$ xdotool search --onlyvisible emacs
94371868

> renderGlobalOptionsF ('GlobalOptions' { 'window' = Just 94371868, 'delay' = Just 100, 'clearmodifiers' = Just 'ClearModifiers' })
GlobalOptions { 'window' = 'Const' ["--window","94371868"], 'delay' = 'Const' ["--delay","100"], 'clearmodifiers' = 'Const' ["--clearmodifiers"] }
@

@doctest@s (TODO):

>>> renderGlobalOptionsF (GlobalOptions { window = Just 94371868, delay = Just 100, clearmodifiers = Just ClearModifiers })
GlobalOptions { window = Const ["--window","94371868"], delay = Const ["--delay","100"], clearmodifiers = Const ["--clearmodifiers"] }

-}

renderGlobalOptionsF :: GlobalOptions Maybe -> GlobalOptions (Const [String])
renderGlobalOptionsF GlobalOptions{ window=mWindow, delay=mDelay, clearmodifiers=mClearModifiers } =
  GlobalOptions
    { window         = (Const sWindow)
    , delay          = (Const sDelay)
    , clearmodifiers = (Const sClearModifiers)
    }
  where
  sWindow          = mWindow         & maybe [] renderWindowOption
  sDelay           = mDelay          & maybe [] renderDelayOption
  sClearModifiers  = mClearModifiers & maybe [] renderClearModifiersOption

--------------------------------------------------

{-| 

-}

renderWindowOption :: WindowID -> [String]
renderWindowOption (WindowID i) = ["--window" , show i]

--------------------------------------------------

{-| 

-}

renderDelayOption :: Milliseconds -> [String]
renderDelayOption (Milliseconds i) = ["--delay" , show i]

--------------------------------------------------

{-| 

-}

renderClearModifiersOption :: ShouldClearModifiers -> [String]
renderClearModifiersOption = \case
  ClearModifiers   -> ["--clearmodifiers"]
  InheritModifiers -> []

--------------------------------------------------
--------------------------------------------------
{-


--------------------------------------------------
NOTE `GlobalOptions` is a higher-order type-constructor,
not a vanilla (i.e. lower-order) unary type-constructor.
i.e. `Show1 GlobalOptions` is ill-kinded;
we need a `ShowF GlobalOptions` (or some such).
ditto `Generic1`, etc.

instance (Show1 f) => Show1 GlobalOptions where

  liftShowsPrec
    :: forall x.
       (Int -> x -> ShowS)
    -> ([x] -> ShowS)
    -> Int
    -> f x
    -> ShowS

  liftShowsPrec = _

instance (Eq1   f) => Eq1   (T f) where ...
instance (Ord1  f) => Ord1  (T f) where ...

instance (Show1 f, Show a) => Show (T f a) where showsPrec = showsPrec1
instance (Eq1   f, Eq   a) => Eq   (T f a) where (==)      = eq1
instance (Ord1  f, Ord  a) => Ord  (T f a) where compare   = compare1

(Lift,Generic)
(NFData,Hashable)

instance (Read1 f) => Read1 (T f) where ...
instance (Read1 f, Read a) => Read (T f a) where
  readPrec     = readPrec1
  readListPrec = readListPrecDefault

--------------------------------------------------


-}
--------------------------------------------------
--------------------------------------------------