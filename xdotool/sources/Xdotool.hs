--------------------------------------------------
--------------------------------------------------

{-|

See <http://manpages.ubuntu.com/manpages/trusty/man1/xdotool.1.html man xdotool>

NOTE ReExports all @.Options@ modules, which use @DuplicateRecordFields@.

-}

module Xdotool
 ( module Xdotool.Core
 
 , module Xdotool.Commands.Key
 , module Xdotool.Commands.Type
 , module Xdotool.Commands.Search

 , module Xdotool.Options.Global
 , module Xdotool.Options.Key
 , module Xdotool.Options.Type
 , module Xdotool.Options.Search

 , Default(..)

 ) where

--------------------------------------------------

import Xdotool.Core

import Xdotool.Commands.Key
import Xdotool.Commands.Type
import Xdotool.Commands.Search

import Xdotool.Options.Global
import Xdotool.Options.Key
import Xdotool.Options.Type
import Xdotool.Options.Search

--------------------------------------------------

import "data-default-class" Data.Default.Class as X (Default(..))

--------------------------------------------------
--------------------------------------------------λ> 