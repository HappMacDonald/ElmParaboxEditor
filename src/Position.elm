module Position exposing
  ( Position
  , Maybe(..)
  , set
  , getX
  , getY
  , getList
  )
import Size exposing (Size)

type alias Integer = Int -- Abbreviations are bad.
type Position =
    Position Integer Integer -- x y

type Maybe =
    Valid Position
  | TooLarge
  | Negative

set : Integer -> Integer -> Size -> Maybe
set x y size =
  if(x>=(Size.getWidth size) || y>=(Size.getHeight size))
    then TooLarge
    else if(x<0 || y<0)
      then Negative
      else Valid <| Position x y

getX : Position -> Integer
getX (Position x _) = x

getY : Position -> Integer
getY (Position _ y) = y

getList : Position -> List Integer
getList (Position x y) =
  [x, y]

