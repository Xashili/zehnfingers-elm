import Keyboard exposing (isDown)
import Char exposing (toCode)
import String exposing (toUpper)
import Html exposing (text)
import List exposing (map, map2, foldr, repeat, sortBy, take, drop, intersperse)
import Signal exposing (map4)
import Color exposing (..)
import Text exposing (..)
import Window exposing (dimensions)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Markdown


-- Keymap

leftKeyMap =
  [ ([True, False, False, False], "a")
  , ([False, True, False, True], "b")
  , ([False, True, False, False], "e")
  , ([True, True, True, False], "g")
  , ([False, True, True, False], "i")
  , ([True, False, False, True], "k")
  , ([False, False, True, True], "o")
  , ([False, True, True, True], "p")
  , ([True, False, True, True], "q")
  , ([False, False, False, True], "t")
  , ([True, True, False, False], "u")
  , ([True, True, True, True], "y")
  , ([True, True, False, True], "z")
  , ([False, False, True, False], " ")
  , ([True, False, True, False], ".")
  ]

rightKeyMap =
  [ ([False, False, True, True], "c")
  , ([False, True, True, False], "d")
  , ([False, True, True, True], "f")
  , ([False, False, False, True], "h")
  , ([True, False, False, True], "j")
  , ([True, True, False, False], "l")
  , ([True, True, True, False], "m")
  , ([True, False, False, False], "n")
  , ([False, False, True, False], "r")
  , ([False, True, False, False], "s")
  , ([True, False, True, True], "v")
  , ([True, True, True, True], "w")
  , ([True, False, True, True], "x")
  , ([False, True, False, True], ",")
  ]

keyMap : List (Bool, List Bool, String)
keyMap =
  let change a (b, c) = (a, b, c)
  in map (change True) leftKeyMap ++ map (change False) rightKeyMap

-- Input
ks chr = isDown (toCode chr)

toBools4 a b c d = [a, b, c, d]
toBools2 a b = [a, b]

left  = Signal.map4 toBools4 (ks 'A') (ks 'S') (ks 'D') (ks 'F')
right = Signal.map4 toBools4 (ks 'J') (ks 'K') (ks 'L') (isDown 186)
mods  = Signal.map2 toBools2 (ks 'C') (ks 'M')

input = Signal.map3 (\a b c -> (a, b, c)) left right mods

-- Model
type alias Keys =
  { leftPressed : List Bool
  , leftDown : Bool
  , leftInput : List Bool
  , rightPressed : List Bool
  , rightDown : Bool
  , rightInput : List Bool
  , modPressed : List Bool
  , modDown : Bool
  , modInput : List Bool
  , input : String
  }

default =
  { leftPressed = [False, False, False, False]
  , leftDown = False
  , leftInput = [False, False, False, False]
  , rightPressed = [False, False, False, False]
  , rightDown = False
  , rightInput = [False, False, False, False]
  , modPressed = [False, False]
  , modDown = False
  , modInput = [False, False]
  , input = "\n"
  }

handle : String -> Bool -> List Bool -> List Bool -> String
handle s left mod keys =
  let
    func (l, list, chr) old = if (l, list) == (left, keys) then chr else old
    key = foldr func "" keyMap
    upKey = case key of
      " " -> "\n"
      "," -> "?"
      "." -> "!"
      otherwise -> String.toUpper key   
  in s ++ if mod == [False, False] then key else upKey

changed : List Bool -> List Bool -> List Bool
changed old new = List.map2 (\o n -> not o && n) old new

update : (List Bool, List Bool, List Bool) -> Keys -> Keys
update (left, right, mod) ks =
  let
    modChanged = changed ks.modInput mod 
    leftChanged = changed ks.leftInput left
    rightChanged = changed ks.rightInput right
  
    new = { leftPressed = map2 (||) leftChanged ks.leftPressed
          , rightPressed = map2 (||) rightChanged ks.rightPressed
          , modPressed = map2 (||) modChanged ks.modPressed
          , leftDown = foldr (||) False left
          , rightDown = foldr (||) False right
          , modDown = foldr (||) False mod
          , leftInput = left
          , rightInput = right
          , modInput = mod
          , input = ks.input }
  in
    if | (not new.leftDown || new.rightDown) && ks.leftDown ->
         { new | input <- handle ks.input True new.modPressed new.leftPressed
               , leftPressed <- repeat 4 False
               , modPressed <- repeat 2 False
               }
       | (not new.rightDown || new.leftDown) && ks.rightDown ->
         { new | input <- handle ks.input False new.modPressed new.rightPressed
               , rightPressed <- repeat 4 False
               , modPressed <- repeat 2 False
               }
       | otherwise -> new


-- Main
main =
  Signal.map2 view dimensions (Signal.foldp update default input)

-- Display
view (w, h) d = flow down
  [ container (widthOf sheet) 40 middle intro
  , sheet
  , leftAligned <| fromString d.input
  , faq ]

displayChord : Bool -> List Bool -> String -> Element
displayChord left bs s =
  flow Graphics.Element.right
    [ container 25 22 middle (centered <| fromString <| String.toUpper s)
    , row (if left then blueSquare else redSquare) bs
    ]

row : Element -> List Bool -> Element
row e bs = flow Graphics.Element.right <|
  List.map2 (\a b -> if a then b else emptySquare) bs (repeat 4 e)

blueSquare : Element
blueSquare =
  collage 22 22 [filled blue square]

redSquare : Element
redSquare =
  collage 22 22 [filled red square]

emptySquare : Element
emptySquare = collage 22 22 []

square : Path
square = 
  path [ (10,10), (10,-10), (-10,-10), (-10,10), (10,10) ]
  
uncurry3 f (a, b, c) = f a b c

intro = centered <| Text.concat
  [ fromString "Write something using "
  , Text.color blue <| bold <| fromString "ASDF"
  , fromString ", "
  , Text.color red <| bold <| fromString "JKL;"
  , fromString " and "
  , Text.color yellow <| bold <| fromString " CM"
  ]

sheet =
  let keys = sortBy (\(a, b, c) -> c) keyMap
      col1 = take 10 keys
      col2 = take 10 (drop 10 keys)
      col3 = take 10 (drop 20 keys)
      col c = flow down <| map (uncurry3 displayChord) c
  in flow Graphics.Element.right <|
     intersperse (spacer 20 20) [ col col1, col col2, col col3 ]

faq = Markdown.toElement """
**What is this**

A short demonstration of an input method for 10 fingers.
It uses *chorded inputs*, so you have to press several keys to get an output.
This has some advantages:
 * your fingers need less movement (for if you are troubled with RSI)
 * a keyboard (or a glove!) with 10 keys is easier to transport
 * it may be better for touch screens
 
But because of its chorded nature, it is probably slower than a keyboard when only typing single characters.

**What is so special?**

Not much.
This is not a novelty, as there exist some chorded keyboards and implementations for a long time — even for 10 fingers.

But the layout presented here has some small differences.
Mainly, it does not allow having mixed chords for letters.
So every chord uses either the left or the right hand.

If you have mixed chords and press a chord with your left hand,
your right hand needs to wait before pressing anything,
as the combination of both could be the mixed chord.

Here you can press DF down, then press J, and afterward release DF.
The implementation knows that your hand is done with a chord as soon a you press any key with the other hand. 
Hopefully this will improve typing speed.

Also, an open source implementation for keyboards is in development.

**Aren't there too few keys?**

You may have two thumbs and eight other fingers.
If not, unfortunately this layout is not suited for you.
The thumbs allow a combination of 2 x 2 = 4 layers.
Only at the two main layers (a-z and A-Z) the mixed chords are disallowed,
giving both 2 x (2^4 - 1) = 30 chords for the home row.
At the other layers, with mixed chords allowed, there are 2^8 - 1 = 255 possibilities.
Plenty enough for numbers, brackets, common words, special characters and more.
But they are not implemented here yet.

Alltogether there are 2x30 + 2x255 = 570 chords.

**This does not work**

 * Please change your system layout to US-QWERTY.

 * Combinations that uses multiple keys may or may not work based on your browser and keyboard.

 * Uppercase letters seem to be bugged anyway.
"""
