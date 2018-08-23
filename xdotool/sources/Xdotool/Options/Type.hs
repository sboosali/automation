--------------------------------------------------
--------------------------------------------------

{-|



@
KEYBOARD COMMANDS

       key
           ...

       keydown
           ...

       keyup
           ...

       type [options] something to type
           Options:

           --window windowid
               Send keystrokes to a specific window id. See "SENDEVENT NOTES" below. The default,
               if no window is given, depends on the window stack. If the window stack is empty
               the current window is typed at using XTEST. Otherwise, the default is "%1" (see
               "WINDOW STACK").

           --delay milliseconds
               Delay between keystrokes. Default is 12ms.

           --clearmodifiers
               Clear modifiers before sending keystrokes. See CLEARMODIFIERS below.

           Types as if you had typed it. Supports newlines and tabs (ASCII newline and tab). Each
           keystroke is separated by a delay given by the --delay option.

           With respect to "COMMAND CHAINING", this command consumes the remainder of the
           arguments and types them. That is, no commands can chain after 'type'.

           Example: to type 'Hello world!' you would do:
            xdotool type 'Hello world!'

@

-}

module Xdotool.Options.Type where

--------------------------------------------------

import Xdotool.Options.Global

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
--------------------------------------------------

{-| @xdotool type ...@ has no command-specific options.

-}

type TypeOptions = GlobalOptions

--------------------------------------------------
--------------------------------------------------