module Size exposing
  ( Size
  , Maybe(..)
  , set
  , getWidth
  , getHeight
  , getList
  , degenerate
  )

type alias Integer = Int -- Abbreviations are bad.

-- Freshness Guarantee monoid for exterior size measurements
type Size =
    Size Integer Integer -- width height

-- Error monoid for exterior size measurements
type Maybe =
    Valid Size
  | Negative

set : Integer -> Integer -> Maybe
set width height =
  if(width<0 || height<0)
    then Negative
    else Valid (Size width height)

getWidth : Size -> Integer
getWidth (Size width _) = width

getHeight : Size -> Integer
getHeight (Size _ height) = height

getList : Size -> List Integer
getList (Size width height)=
  [width, height]

degenerate : Size
degenerate = Size 0 0