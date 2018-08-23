--------------------------------------------------
--------------------------------------------------

{-|



@
KEYBOARD COMMANDS

       type
           ...

       key [options] keystroke [keystroke ...]
           Options:

           --window window
               Send keystrokes to a specific window id. You can use "WINDOW STACK" references
               like "%1" and "%@" here. If there is a window stack, then "%1" is the default,
               otherwise the current window is used.

               See also: "SENDEVENT NOTES" and "WINDOW STACK"

           --clearmodifiers
               Clear modifiers before sending keystrokes. See CLEARMODIFIERS below.

           --delay milliseconds
               Delay between keystrokes. Default is 12ms.

           Type a given keystroke. Examples being "alt+r", "Control_L+J", "ctrl+alt+n",
           "BackSpace".

           Generally, any valid X Keysym string will work. Multiple keys are separated by '+'.
           Aliases exist for "alt", "ctrl", "shift", "super", and "meta" which all map to Foo_L,
           such as Alt_L and Control_L, etc.

           In cases where your keyboard doesn't actually have the key you want to type, xdotool
           will automatically find an unused keycode and use that to type the key.

           With respect to "COMMAND CHAINING", this command consumes the remainder of the
           arguments or until a new xdotool command is seen, because no xdotool commands are
           valid keystrokes.

           Example: Send the keystroke "F2"
            xdotool key F2

           Example: Send 'a' with an accent over it (not on english keyboards, but still works
           with xdotool)
            xdotool key Aacute

           Example: Send ctrl+l and then BackSpace as separate keystrokes:
            xdotool key ctrl+l BackSpace

           Example: Send ctrl+c to all windows matching title 'gdb' (See "COMMAND CHAINING")
            xdotool search --name gdb key ctrl+c

       keydown [options] keystroke
           Same as above, except only keydown (press) events are sent.

       keyup keystroke
           Same as above, except only keyup (release) events are sent.

@

-}

module Xdotool.Options.Key where

--------------------------------------------------

import Xdotool.Options.Global

--------------------------------------------------

import "base" Text.Show
import "base" Data.Functor.Classes

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-| @xdotool key ...@ has no command-specific options.

-}

type KeyOptions = GlobalOptions

--------------------------------------------------

{-| @xdotool keyup ...@ has no command-specific options.

-}

type KeyUpOptions = GlobalOptions

--------------------------------------------------

{-| @xdotool keydown ...@ has no command-specific options.

-}

type KeyDownOptions = GlobalOptions

--------------------------------------------------
--------------------------------------------------