module OrderedSequence exposing (OrderedSequence, create, unpack)
import List.Extra

-- This monoid is basically just a uniqueness guarantee for a list
type OrderedSequence a =
  OrderedSequence (List a)

create : List a -> OrderedSequence a
create list =
  OrderedSequence (List.Extra.unique list)

unpack : OrderedSequence a -> List a
unpack (OrderedSequence list) = list
