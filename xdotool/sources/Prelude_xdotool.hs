----------------------------------------
----------------------------------------

module Prelude_xdotool
 ( module X
 , module Prelude_xdotool
 ) where

----------------------------------------
----------------------------------------

import "base" Text.Show            as X (ShowS)
import "base" Data.Functor.Classes as X (Show1,Eq1,Ord1)

----------------------------------------

import "spiros" Prelude.Spiros as X

----------------------------------------
----------------------------------------

import "base" Text.Show            as X (showsPrec)
import "base" Data.Functor.Classes as X (showsPrec1)

----------------------------------------
----------------------------------------

-- | Help show an alphanumeric constructor (i.e. a function, not an operator).
--
-- (Helper for manually writing 'Show'\/'Show1' instances)
--
showAlphanumericConstructor :: Int -> [ShowS] -> ShowS
showAlphanumericConstructor precedence strings = 
    showParen (precedence >= subexpressionPrecedence)
      (foldShowS strings)

-- | 'showsPrec1' for the syntactic-context of a subexpression whose outermost "sub-sub-expression" is a function application (versus an operator application or a builtin).
--
-- @showsSubPrec1 = 'showsPrec1' 'applicationPrecedence'@
--
-- (Helper for manually writing 'Show'\/'Show1' instances)
--
showsSubPrec1 :: forall f a. (Show1 f, Show a) => f a -> ShowS
showsSubPrec1 = showsPrec1 applicationPrecedence

    --Note--
    --
    -- e.g. `applicationPrecedence` versus `subexpressionPrecedence`:
    --
    -- applicationPrecedence
    --     → { window = (Const ["--window","94371868"]), ... }
    --
    -- subexpressionPrecedence
    --     → { window = Const ["--window","94371868"], ... }
    --

-- |
--
-- @showsSubPrec = 'showsPrec' 'applicationPrecedence'@
--
-- (Helper for manually writing 'Show'\/'Show1' instances)
--
showsSubPrec :: forall a. (Show a) => a -> ShowS
showsSubPrec = showsPrec applicationPrecedence

-- | Specialized chained function-composition.
--
-- (Helper for manually writing 'Show'\/'Show1' instances)
--
foldShowS :: [ShowS] -> ShowS
foldShowS = foldr (.) id

-- | An arbitrary(?) precedence having a higher precedence than 'applicationPrecedence'.
--
-- (Helper for manually writing 'Show'\/'Show1' instances)
--
subexpressionPrecedence :: Int
subexpressionPrecedence = 1 + applicationPrecedence

-- | The operator-precedence of function-application.
--
-- (Helper for manually writing 'Show'\/'Show1' instances)
--
applicationPrecedence :: Int
applicationPrecedence = 10

----------------------------------------
----------------------------------------λ> λ> 