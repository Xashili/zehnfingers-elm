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
mod = and (keysToKeyset [4, 5])
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
  
normalList = flipHands
  [ ("n", [3])
  , ("t", [2])
  , ("s", [2, 3])  
  , ("m", [1])
  , ("y", [1, 3])
  , ("l", [1, 2])  
  , ("b", [1, 2, 3])
  , ("c", [0])
  , ("f", [0, 3])
  , ("h", [0, 2])
  , ("w", [0, 2, 3])
  , ("g", [0, 1])
  , ("j", [0, 1, 3])
  , ("d", [0, 1, 2])
  , ("v", [0, 1, 2, 3])
  , ("i", [9])
  , ("u", [8])  
  , ("z", [8, 9])
  , (" ", [7])  
  , ("o", [7, 9])
  , ("e", [7, 8])  
  , (",", [7, 8, 9])
  , ("r", [6])
  , ("k", [6, 9])
  , (".", [6, 8])
  , ("q", [6, 8, 9])
  , ("a", [6, 7])
  --, ("", [6, 7, 9])
  , ("p", [6, 7, 8])
  , ("x", [6, 7, 8, 9])  
  ]

specialList = map (\(s, k) -> (s, 4::5::k))
  [ ("(", [3])
  , ("[", [2])
  , ("@", [2, 3])  
  , ("{", [1])
  , ("+", [1, 3])
  , ("-", [1, 2])
  , ("\\", [1, 2, 3])
  , ("\"", [0])
  , ("[", [0, 3])
  , ("<", [0, 2])
  , ("", [0, 2, 3])
  , (":", [0, 1])
  , ("&", [0, 1, 3])
  , ("=", [0, 1, 2])
  , ("^", [0, 1, 2, 3])
  , (")", [6])
  , ("]", [7])
  , ("$", [7, 6])  
  , ("}", [8])
  , ("*", [8, 6])
  , ("_", [8, 7])  
  , ("/", [8, 7, 6])
  , ("\'", [9])
  , ("]", [9, 6])
  , (">", [9, 7])
  , ("", [9, 7, 6])
  , (";", [9, 8])
  , ("|", [9, 8, 6])
  , ("~", [9, 8, 7])
  , ("%", [9, 8, 7, 6])
  ] 

metaList = map (\(s, k) -> (s, 5::k))
  [ ("DEL", [2])
  , ("0", [6])
  , ("1", [7])
  , ("4", [7, 6])  
  , ("2", [8])
  , ("5", [8, 7])  
  , ("7", [8, 7, 6])
  , ("3", [9])
  , ("6", [9, 8])
  , ("8", [9, 8, 7])
  , ("9", [9, 8, 7, 6])
  ]

shiftList = map (\(s, k) -> (shift s, 4::k)) normalList

flipHands : List (String, List Int) -> List (String, List Int)
flipHands = map (\(s, ks) -> (s, map (\k -> 9 - k) ks))

shift : String -> String
shift s = case s of
  " " -> "\n"
  "," -> "?"
  "." -> "!"
  otherwise -> String.toUpper s

keyMap : Dict.Dict Int String
keyMap = Dict.fromList <| map (\(char, keys) -> (keysToKeyset keys, char)) <| 
  (normalList ++ shiftList ++ specialList ++ metaList)
  

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
  Just a -> case a of
    "DEL" -> String.slice 0 -1 s
    otherwise -> s ++ a

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
view (w, h) d =
  let list = case mod d.input of
        0 -> normalList
        16 -> shiftList
        32 -> specialList
        48 -> metaList
      s = sheet list
  in flow down
  [ container (widthOf (sheet normalList)) 40 middle intro
  , sheet list
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
     [ container 30 22 middle (centered <| fromString s)
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

sheet list = 
  let keys = sortBy (\(a, c) -> a) list
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

