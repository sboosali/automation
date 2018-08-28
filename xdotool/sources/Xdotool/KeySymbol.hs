--------------------------------------------------
--------------------------------------------------

{-|


-}

module Xdotool.KeySymbol where

--------------------------------------------------

import Prelude_xdotool

--------------------------------------------------
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

{-| All keysyms:

@
space
R
exclam
S
quotedbl
T
numbersign
U
dollar
V
percent
W
ampersand
X
quoteright
Y
parenleft
Z
parenright
bracketleft
asterisk
backslash
plus
bracketright
comma
asciicircum
minus
underscore
period
quoteleft
slash
a
0
b
1
c
2
d
3
e
4
f
5
g
6
h
7
i
8
j
9
k
colon
l
semicolon
m
less
n
equal
o
greater
p
question
q
at
r
A
s
B
t
C
u
D
v
E
w
F
x
G
y
H
z
I
braceleft
J
bar
K
braceright
L
asciitilde
M
ctrl (Control key)
N
alt (Alternate key)
O
BackSpace
P
F1-F12 (Function 1 key to Function 12 just use the number of the specific Function key)
Q
Return (Return or Enter key)
@

-}


--------------------------------------------------






--------------------------------------------------
--------------------------------------------------λ> λ> λ> 