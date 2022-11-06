module HSVColor exposing
  ( HSVColor
  , Record
  , unitClamp
  , fromHSV1
  , fromRecord
  , toRecord
  , toCssString
  , toColor
  , fromColor
  , black
  )
import Color exposing (Color)
import Basics.Extra


type alias Integer = Int -- Abbreviations are bad.
type alias Record =
  { hue: Float
  , saturation: Float
  , value: Float
  }


type HSVColor = HSVColor Record


unitClamp : Float -> Float
unitClamp input =
  input
  |> Basics.Extra.atMost 1
  |> Basics.Extra.atLeast 0


fromHSV1 : Float -> Float -> Float -> HSVColor
fromHSV1 hue saturation value =
  HSVColor
    { hue = hue
    , saturation = saturation
    , value = value
    }


fromRecord : Record -> HSVColor
fromRecord record = HSVColor record


toRecord : HSVColor -> Record
toRecord (HSVColor record) = record


toCssString : HSVColor -> String
toCssString hsvColor =
  hsvColor
  |> toColor
  |> Color.toCssString


-- conversion code derived from https://stackoverflow.com/a/54024653/1440906
toColor : HSVColor -> Color
toColor ( HSVColor record ) =
  let
    {hue, saturation, value} = record

    valueAtHue(sampleHue) =
      let
        inverseTruncatedTrianglePhase =
          (5/6) -- inverse inflection point = violet @ 300 degrees
          - sampleHue + hue
          |> Basics.Extra.fractionalModBy 1 -- Normalize the phase

        inverseTruncatedTriangleMagnitude =
          min
            inverseTruncatedTrianglePhase
            (4/6) - inverseTruncatedTrianglePhase
          |> unitClamp

      in
      ( value
      * ( 1 - saturation * inverseTruncatedTriangleMagnitude )
      )
  in
  { red = valueAtHue 0/6  -- 0 degrees
  , green = valueAtHue 2/6  -- 120 degrees
  , blue = valueAtHue 4/6  -- 240 degrees
  -- Our implementation currently only deals with fully opaque colors
  , alpha = 1
  }
  |> Color.fromRgba


-- Test failing until I get to the point where this is helpful.
fromColor : Color -> HSVColor
fromColor color =
  let
    {red, green, blue} = Color.toRgba color
  in
    fromHSV1 0.5 0.5 0.5


black : HSVColor
black = fromHSV1 0 0 0