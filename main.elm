import Keyboard exposing (keysDown)
import Char exposing (fromCode)
import String exposing (toUpper)
import Html exposing (text)
import List exposing (map, map2, foldr, repeat, sortBy, take, drop, intersperse)
import Color exposing (..)
import Text exposing (..)
import Window exposing (dimensions)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Markdown
import Time exposing (..)
import Bitwise exposing (..)
import Dict exposing (fromList, get)
import Set exposing (toList)

{-- Time you have to change a chord before it gets registered --}
toWait = 60 * millisecond 

{-- A Keyset is a bitmap from 0..9 --}
type alias Keyset = Int
type alias Key = Int

{-- Helper function for accessing keysets --}
left = and (keysToKeyset [0, 1, 2, 3])
right = and (keysToKeyset [6, 7, 8, 9])
leftMod = and (keysToKeyset [0, 1, 2, 3, 4, 5])
rightMod = and (keysToKeyset [4, 5, 6, 7, 8, 9])

charsToKeyset : List Char -> Keyset
charsToKeyset = foldr (\k old -> old + (1 `shiftLeft` charToIndex k)) 0

keysToKeyset : List Key -> Keyset
keysToKeyset = foldr (\k old -> old + (1 `shiftLeft` k)) 0

{-- If you want different keyboard keys for input, change it here --}
charToIndex : Char -> Int
charToIndex c = case c of
  'Q' -> 0
  'W' -> 1
  'E' -> 2
  'R' -> 3

  'C' -> 4
  'M' -> 5

  'U' -> 6
  'I' -> 7
  'O' -> 8
  'P' -> 9
  
  otherwise -> 10
  
{--leftKeyMap =
  [ ("a", [0])
  , ("c", [1])
  , ("h", [1, 0])  
  , ("u", [2])
  , ("q", [2, 0])
  , ("s", [2, 1])  
  , ("t", [1, 2, 0])
  , ("y",  [3])
  , ("v", [0, 3])
  , ("x", [3, 1])
  , ("z", [0, 1, 3])
  , ("j", [3, 2])
  , ("k", [0, 2, 3])
  , (".", [3, 1, 2])
  , ("i", [0, 1, 2, 3])
  ]--}

rightKeyMap =
  [ ("i", [6])
  , ("y", [7])  
  , (",", [6, 7])
  , ("e", [8])  
  , ("d", [8, 6])
  , ("g", [8, 7])  
  , ("n", [7, 8, 6])
  , ("x", [9])
  , ("b", [6, 9])
  , (".", [9, 7])
  --, ("", [6, 7, 9])
  , ("p", [9, 8])
  , ("l", [6, 8, 9])
  , ("r", [9, 7, 8])
  , ("a", [6, 7, 8, 9])  
  ]
  
leftKeyMap =
  [ ("c", [3])
  , (" ", [2])
  , ("h", [2, 3])  
  , ("u", [1])
  , ("q", [1, 3])
  , ("s", [1, 2])  
  , ("t", [1, 2, 3])
  , ("v", [0])
  , ("k", [0, 3])
  , ("f", [0, 2])
  , ("w", [0, 2, 3])
  , ("j", [0, 1])
  , ("z", [0, 1, 3])
  , ("m", [0, 1, 2])
  , ("o", [0, 1, 2, 3])
  ]
{--
rightKeyMap =
  [ ("e", [9])
  , ("o", [8])  
  , ("h", [8, 9])
  , ("u", [7])  
  , ("s", [7, 9])
  , ("p", [7, 8])  
  , ("t", [7, 8, 9])
  , ("q", [6])
  , ("d", [6, 9])
  , ("f", [6, 8])
  , ("w", [6, 8, 9])
  , ("y", [6, 7])
  , (",", [6, 7, 9])
  , ("b", [6, 7, 8])
  , (" ", [6, 7, 8, 9])  
  ]
--}

keyList = flipHands <| leftKeyMap ++ rightKeyMap

flipHands : List (String, List Int) -> List (String, List Int)
flipHands = map (\(s, ks) -> (s, map (\k -> 9 - k) ks))

keyMap : Dict.Dict Int String
keyMap =
  let keys = keyList
      upper1 = map (\(s, k) -> (String.toUpper s, 4::k)) keys
      upper2 = map (\(s, k) -> (String.toUpper s, 5::k)) keys
  in Dict.fromList <| map (\(char, keys) -> (keysToKeyset keys, char)) (keys ++ upper1 ++ upper2)
  

{--
 Model
--}
type Side = Left | Neither | Right 

type alias Model =
  { input : Keyset
  , text : String
  , last : Keyset
  , time : Time
  , active : Side
  , lastActive : Side
  }

defaultModel =
  { input = 0
  , text = "\n"
  , last = 0
  , time = 0
  , active = Neither
  , lastActive = Neither
  }

{--
 Update
--}
getSideMod side = case side of
    Left -> leftMod
    Right -> rightMod
    Neither -> and 0

getSide side = case side of
  Left -> left
  Right -> right
  Neither -> and 0

print : String -> Side -> Keyset -> String
print s side keys = case get ((getSideMod side) keys) keyMap of
  Nothing -> s
  Just a -> s ++ a

updateKeys : (Time, Keyset) -> Model -> Model
updateKeys (time, keys) old =
  let
    delta = time - old.time
    changed = keys `Bitwise.xor` old.input
  
    new = { input = keys
          , last = if old.input == 0 then keys else old.last
          , active = if | left changed > 0 -> Left
                        | right changed > 0 -> Right
                        | otherwise -> Neither
          , lastActive = old.lastActive
          , time = time
          , text = old.text }    
  -- handle hand switch:
  -- if a hand leaves key while active, print the last chord
  -- if a hand is active, print the other one
  in if | new.active == old.lastActive && 
          (getSide new.active) keys == 0 && delta < toWait ->
          { new | text <- print old.text new.active new.last
                , last <- keys
                , active <- new.active }
        | not (new.active == old.active) && delta < toWait ->
          { new | text <- print old.text old.active old.input
                , last <- keys
                , active <- new.active }
        | otherwise -> new

{-- After toWait milliseconds the chord gets printed,
    if it hasn't changed --}
updatePrint : (Time, Keyset) -> Model -> Model
updatePrint (time, input) old = if not (time == old.time) then old else
  { old | text <- print old.text old.active old.input
        , last <- input
        , active <- old.lastActive}


update : Input -> Model -> Model
update i m = case i of
  Keys info -> updateKeys info m
  Print info -> updatePrint info m

{--
 Main
--}
main =
  Signal.map2 view dimensions a

a : Signal Model
a = Signal.foldp update defaultModel input

{--
 Input
--}
type Input = Print (Time, Keyset) | Keys (Time, Keyset)

rawInput : Signal (Time, Keyset)
rawInput = timestamp <|
  Signal.map (charsToKeyset << map Char.fromCode << Set.toList) keysDown

-- The Print events follow the Keys event after a delay of toWait
input = 
  let keys = Signal.map Keys rawInput
      prints = delay toWait <| Signal.map Print rawInput
  in Signal.merge keys prints

{--
 Display
--}
view (w, h) d = flow down
  [ container (widthOf sheet) 40 middle intro
  , sheet
  , leftAligned <| fromString d.text
  , faq ]

keysetToList : Keyset -> (Bool, List Bool)
keysetToList ks =
  let left = ks < 1 `shiftLeft` 4
      list = map (\i -> if left then i else i + 6) [0, 1, 2, 3] |>
             map (\i -> (1 `shiftLeft` i) `and` ks > 0)
  in (left, list)

displayChord : String -> List Int -> Element
displayChord s keys =
  let bs = map (\x -> List.member x keys) [0,1,2,3,6,7,8,9]
      left = List.any identity (take 4 bs)
      bs' = (if left then take 4 else drop 4) bs
      square = path [ (10,10), (10,-10), (-10,-10), (-10,10), (10,10) ]
      blueSquare = collage 22 22 [filled blue square]
      redSquare = collage 22 22 [filled red square]
  in flow Graphics.Element.right
     [ container 25 22 middle (centered <| fromString <| String.toUpper s)
     , row (if left then blueSquare else redSquare) bs'
     ]

row : Element -> List Bool -> Element
row e bs = flow Graphics.Element.right <|
  List.map2 (\a b -> if a then b else collage 22 22 []) bs (repeat 4 e)
  
intro = centered <| Text.concat
  [ fromString "Write something using "
  , Text.color blue <| bold <| fromString "QWER"
  , fromString ", "
  , Text.color red <| bold <| fromString "UIOP"
  , fromString " and "
  , Text.color yellow <| bold <| fromString " CM"
  ]

sheet = 
  let keys = sortBy (\(a, c) -> a) keyList
      col1 = take 10 keys
      col2 = take 10 (drop 10 keys)
      col3 = take 10 (drop 20 keys)
      col c = flow down <| map (uncurry displayChord) c
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
 
But because of its chorded nature, it is probably slower than a keyboard when only 
typing single characters.

**How do I type?**

The demo registers the last active chord if
 * the same chord is hold longer then 60 milliseconds,
 * the other hand presses anything, or
 * the fingers of the last active hand leaves all keys

Most chorded layouts only depend on the last point.
But this is slow when processing single characters (and not [words](http://openstenoproject.org/)).

If typing english words with this layout,
half of the time you can speed up by alternating hands.
The time limit makes the setup prone to errors,
but a good implementation could figure out via try and error,
how much time you need to change a chord.

The big plus on the other hand is,
that you can do roll-overs.
Want to type the common “to”?
Hold UIO down, then press P. And you are done!
You can try to find long combinations to form words,
but it is suggested to raise the keys every second chord.
Instead of typing “rain” in one go, write “ra” and then “in”.

The layout was optimized to:
 * switch hands as much as possible
 * if characters are on the same hand, try to give them an increasing (press more keys) or decreasing 
(leave some keys) combination of keys 
 * and not a complicated one, where a hand needs to raise *and* press
 * give more common characters easier chords

This means that complicated combinations
should be handled as leaving all keys from the first chord,
then pressing the second. 
Otherwise, at least at the beginning,
you will make many mistakes because of the time limit.

For the english corpus I used this comes down to:
* 47% hand switches between letters
* 27% increasing combos
* 21% decreasing combos
* 5% complicated combos


**Aren't there too few keys?**

You may have two thumbs and eight other fingers.
If not, unfortunately this layout is not suited for you.
The thumbs allow a combination of 2 x 2 = 4 layers.
Only at the two main layers (a-z and A-Z) the left and right mixed chords are disallowed,
giving both 2 x (2^4 - 1) = 30 chords for the home row.
At the other layers, with mixed chords allowed, there are 2^8 - 1 = 255 
possibilities.
Plenty enough for numbers, brackets, common words, special characters and more.
But they are not implemented here yet.

Alltogether there are 2x30 + 2x255 = 570 chords.

**This does not work!**

 * Please change your system layout to US-QWERTY.

 * Combinations that uses multiple keys may or may not work based on your browser 
and keyboard.

 * Uppercase letters seem to be bugged anyway.
"""

