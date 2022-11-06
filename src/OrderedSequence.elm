module OrderedSequence exposing
  ( OrderedSequence
  , create
  , unpack
  , empty
  , cons
  )
import List.Extra

-- This monoid is basically just a uniqueness guarantee for a list
type OrderedSequence a =
  OrderedSequence (List a)

create : List a -> OrderedSequence a
create list =
  OrderedSequence (List.Extra.unique list)

unpack : OrderedSequence a -> List a
unpack (OrderedSequence list) = list


empty : OrderedSequence a
empty = OrderedSequence []


cons : a -> OrderedSequence a -> OrderedSequence a
cons head (OrderedSequence tail) =
  OrderedSequence (head :: tail)
