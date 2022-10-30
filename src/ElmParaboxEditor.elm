module ElmParaboxEditor exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import File.Download as Download
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events exposing (onClick)
import Task exposing (Task)
import Size exposing (Size)
import Position exposing (Position)
import OrderedSequence exposing (OrderedSequence)
import HSVColor exposing (HSVColor)
import List
-- import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)

-- Fixing some elm defects via metalanguage attempts :P
type alias Boolean = Bool -- Abbreviations are bad.
type alias Integer = Int -- Abbreviations are bad.

notANumber : Float
notANumber = 0/0

positiveInfinity : Float
positiveInfinity = 1/0

negativeInfinity : Float
negativeInfinity = -1/0

stringToFloatIncludingSignals : String -> Maybe Float
stringToFloatIncludingSignals inputString =
  let
    maybeNonSignalingFloat = String.toFloat inputString
  in
    case maybeNonSignalingFloat of
      Just nonSignalingFloat -> maybeNonSignalingFloat
      Nothing -> -- Check for in-band signaling constants, case sensitive.
        case inputString of
          "NaN" -> Just notANumber
          "Infinity" -> Just positiveInfinity
          "-Infinity" -> Just negativeInfinity
          default -> Nothing
-- End fixing


type PushAttempt = Push | Enter | Eat | Possess
getPushAttemptString : PushAttempt -> String
getPushAttemptString pushAttempt =
  case pushAttempt of
    Push -> "push"
    Enter -> "enter"
    Eat -> "eat"
    Possess -> "Possess"


pushAttemptNormal : OrderedSequence PushAttempt
pushAttemptNormal =
  OrderedSequence.create [Push, Enter, Eat, Possess]


pushAttemptPriority : OrderedSequence PushAttempt
pushAttemptPriority =
  OrderedSequence.create [Enter, Eat, Push, Possess]



type DrawStyle = DrawStyleTUI | DrawStyleGrid | DrawStyleOldStyle
getDrawStyleString : DrawStyle -> String
getDrawStyleString drawStyle =
  case drawStyle of
    DrawStyleTUI -> "tui"
    DrawStyleGrid -> "grid"
    DrawStyleOldStyle -> "oldstyle"


type FloorButtonType = FloorButton | FloorPlayerButton
getFloorButtonTypeString : FloorButtonType -> String
getFloorButtonTypeString floorButtonType =
  case floorButtonType of
    FloorButton -> "Button"
    FloorPlayerButton -> "PlayerButton"


type PlayerFlag = PlayerFlag Boolean
type PossessableFlag = PossessableFlag Boolean
type Shed = Shed Boolean
type InnerPush = InnerPush Boolean
type FlipH = FlipH Boolean


type alias PlayerOptions =
    { player : PlayerFlag
    , possessable : PossessableFlag
    , playerorder : Integer
    }


type RootBlock =
    RootBlock -- No relevant x/y information
    { id: Integer
    , size: Size
    , color: HSVColor
    , zoomFactor: Float
    -- Presently not allowing root block to be filled with walls
    , playerOptions: PlayerOptions
    , fliph: FlipH
    , specialeffect: Integer
    , children: List GameObject
    }


type Block =
    Block
    { id: Integer
    , position: Position
    , size: Size
    , color: HSVColor
    , zoomFactor: Float
    -- Use different "FillWithWalls" object if you want that flag set.
    , playerOptions: PlayerOptions
    , fliph: FlipH
    , specialeffect: Integer
    , children: List GameObject
    }
  | InSpaceBlock RootBlock


type GameObject =
    HollowBlock Block
  | FillWithWalls -- Block with this flag set true
    { id: Integer
    , position: Position
    , color: HSVColor
    , zoomFactor: Float
    , playerOptions: PlayerOptions
    , fliph: FlipH
    , specialeffect: Integer
    }
  | Ref
    { id: Integer
    , position: Position
    , exitblock: Integer
    , infexit: Integer
    , infexitnum: Integer
    , infenter: Integer
    , infenternum: Integer
    , infenterid: Integer
    , playerOptions: PlayerOptions
    , fliph: FlipH
    , specialeffect: Integer
    }
  | InSpaceRef
    { id: Integer
    , exitblock: Integer
    , infexit: Integer
    , infexitnum: Integer
    , infenter: Integer
    , infenternum: Integer
    , infenterid: Integer
    , playerOptions: PlayerOptions
    , fliph: FlipH
    , specialeffect: Integer
    }
  | Wall
    { position: Position
    , playerOptions: PlayerOptions
    }
  | Floor
    { position: Position
    , floorType: FloorButtonType
    }


type Level =
  Version4
  { attemptOrder : OrderedSequence PushAttempt
  , shed : Shed
  , innerPush : InnerPush
  , drawStyle : DrawStyle
  , customLevelMusic : Integer
  , customLevelPalette : Integer
  , level : List RootBlock
  }


type ParseValid payload =
    ParseValid payload String


type alias MaybeParse payload = Maybe (ParseValid payload)


parseChompAndWrap : String -> Regex -> payload -> ParseValid payload
parseChompAndWrap inputString regex object =
  ParseValid
    object
    ( inputString
    |>Regex.replace regex (always "")
    )


discardWhiteSpace : ParseValid payload -> MaybeParse payload
discardWhiteSpace (ParseValid payload input) =
  String.trimLeft input
  |>ParseValid payload
  |>Just -- MaybeParse


-- Only skips whitespace or explicit comments until it gets to
-- data on a real data line.
discardStartingWhiteSpace : ParseValid payload -> MaybeParse payload
discardStartingWhiteSpace (ParseValid payload input) =
  let
    regexNoDataThisLine =
      Regex.fromString "^@|\\n"
      |>Maybe.withDefault Regex.never

    trimmedInput = String.trimLeft input
  in
    if trimmedInput |> Regex.contains regexNoDataThisLine
    then
      input
      |>discardExplicitComments
      |>discardStartingWhiteSpace -- recurse on following line

    else
      Just (ParseValid payload trimmedInput)


discardImplicitComments : ParseValid payload -> MaybeParse payload
discardImplicitComments (ParseValid payload input) =
  let
    implicitCommentsRegex : Regex
    implicitCommentsRegex =
      Regex.fromString "^.*(?:\\n|$)"
      |>Maybe.withDefault Regex.never
  in
    Regex.replace implicitCommentsRegex (always "") input
    |> ParseValid payload
    |>Just -- MaybeParse


-- Skips implicit comments on current line,
-- and then any explicit comments or whitespace-only lines
-- AND start-of-line whitespace until it gets back to data on a real data line.
skipToNextRealData : ParseValid payload -> MaybeParse payload
skipToNextRealData parseInput =
  parseInput
  |>discardImplicitComments
  |>discardStartingWhiteSpace


discardExplicitComments : ParseValid payload -> MaybeParse payload
discardExplicitComments (ParseValid payload input) =
  let
    atSignRegex : Regex
    atSignRegex =
      Regex.fromString "^\\s*(?=@|\\n|$)"
      |>Maybe.withDefault Regex.never
  in
    if input |> Regex.contains atSignRegex
      then
        input
        |>Regex.replace atSignRegex (always "")
        |>ParseValid payload
        |>discardImplicitComments -- MaybeParse
      else
        input -- no match
        |>ParseValid payload
        |>Just -- MaybeParse


integerRegexString : String
integerRegexString = "^\\s*([+-]?\\d+)\\b"


floatRegexString : String
floatRegexString =
  ( "^\\s*" -- anchor to beginning and ignore trailing whitespace
  ++  "(" -- begin capture group
  ++    "[+-]?" -- optional sign
  --  Form with at minimum one digit left of possible decimal point
  ++    "\\d+(?:[.]\\d*)?"
  ++      "(?:[eE][+-]?\\d+)?" -- also a possible exponent
  ++  "|" -- or..
  --  Form with at minimum decimal point AND one digit to the right
  ++    "[.]\\d+"
  ++      "(?:[eE][+-]?\\d+)?" -- also a possible exponent
  ++  "|NaN|-?Infinity" -- or signaling constants
  ++  ")" -- close capture group
  ++  "(?<=[Ny0-9.])(?=[^a-zA-Z0-9.+-]|$)" -- complex word boundary condition
  )


zeroOneBooleanRegexString : String
zeroOneBooleanRegexString = "^\\s*([01])\\b"


parseInteger : ParseValid oldPayload -> MaybeParse (oldPayload, Integer)
parseInteger parseInput =
    parseInput
    |>parseThing integerRegexString String.toInt


parseZeroOne : (Boolean -> newPayloadType) -> ParseValid oldPayload -> MaybeParse (oldPayload, newPayloadType)
parseZeroOne monoidWrapper parseInput =
  let
    zeroOneStringToMaybeBoolean :  String -> Maybe Boolean
    zeroOneStringToMaybeBoolean zeroOneString =
      case zeroOneString of
        "0" -> Just False
        "1" -> Just True
        _ -> Nothing
  in
    parseInput
    |>parseThing
        zeroOneBooleanRegexString
        zeroOneStringToMaybeBoolean
    |>maybeParseMap monoidWrapper


-- Does a non-maybe operation to the payload of a MaybeParse,
-- returns an identical MaybeParse but with just the payload changed.
-- Namely, remaining parse text gets carried smoothly along.
maybeParseMap : (oldPayload -> newPayload) -> MaybeParse oldPayload -> MaybeParse newPayload
maybeParseMap lambda parseInput =
  case parseInput of
    Nothing -> Nothing
    Just (ParseValid payload remainder) ->
      Just (ParseValid (lambda payload) remainder)


-- I'm leaning toward "won't need this one" due to one above..
-- .. but leaving unfinished bit here in case I waffle back again.
-- maybeParsePayloadCombine : ((a, b) -> c) -> MaybeParse (a, b) -> MaybeParse c
-- maybeParsePayloadCombine combiner input =



-- This kills any possible remaining parse text metadata btw, lol
-- Might make more strict later and fail on non-empty text *shrugs*
parseStripper : MaybeParse payload -> Maybe payload
parseStripper parseInput =
  case parseInput of
    (ParseValid payload _) -> Just payload
    Nothing -> Nothing


-- checks for substring
-- `toMatch` input gets anchored ^ on left for convenience
-- , but is otherwise already considered a regex string.
parseLandmarkCaseInsensitive : String -> ParseValid oldPayload -> MaybeParse oldPayload
parseLandmarkCaseInsensitive toMatch (ParseValid oldPayload toParse) =
  let
    regex =
      Regex.fromString "^"+ toMatch
      |>Maybe.withDefault Regex.never

  in
  if toParse |> Regex.contains regex
    then
      let
        parsed =
          toParse |> Regex.replace regex (always "")
      in
        Just (ParseValid oldPayload parsed)
    else
      Nothing

parseFloat : ParseValid oldPayload -> MaybeParse (oldPayload, Float)
parseFloat parseInput =
  parseInput
  |>parseThing floatRegexString stringToFloatIncludingSignals


parseThing :
  String
  -> (String -> Maybe newPayload)
  -> ParseValid oldPayload
  -> MaybeParse (oldPayload, newPayload)
parseThing regexString toThingLambda (ParseValid payload input) =
  let
    regex =
      Regex.fromString regexString
      |>Maybe.withDefault Regex.never
    matches =
      Regex.find regex input
  in
    case matches of
      [{submatches}] ->
        case submatches of
          [] -> Nothing -- MaybeParse
          Nothing :: _ -> Nothing -- MaybeParse
          Just rawMatchString :: _ ->
            case toThingLambda rawMatchString of
              Nothing -> Nothing -- MaybeParse
              Just matchedResult ->
                parseChompAndWrap
                  input regex (payload, matchedResult)
                |>Just -- MaybeParse
      default -> Nothing -- MaybeParse


parseOneOf :
  List (ParseValid oldPayload -> MaybeParse newPayload)
  -> ParseValid oldPayload
  -> MaybeParse (oldPayload, newPayload)
parseOneOf thingsToTry oldParseResult =
  case thingsToTry of
    [] -> Nothing -- all things to try have failed. D:
    firstThingToTry :: restOfThingsToTry ->
      let
        (ParseValid oldPayload _) =
          oldParseResult

        firstNewResult =
          firstThingToTry oldPayload
      in
        case firstNewResult of
          Nothing -> -- try remainder of things.
            parseOneOf restOfThingsToTry oldPayload

          Just (ParseValid newPayload newParseString) ->
            Just
              ParseValid
              (oldPayload, newPayload)
              newParseString




-- parseListRootBlocks : Parser (List RootBlock)
-- parseListRootBlocks =
--   Parser.loop [] parseRootBlock


-- parseRootBlock :
--   List RootBlock
--   -> Parser
--      ( Parser.Step (List RootBlock) (List RootBlock)
--      )
-- parseRootBlock reversedListOfRootBlocks =
--   Parser.oneOf
--   [ Parser.succeed
--       (\rootblock ->
--           Parser.Loop (rootblock :: reversedListOfRootBlocks)
--       )
--     -- |>Parser.oneOf
--     --   [
--     --   ]
--   , discardExplicitComments
--   , Parser.end
--     |>Parser.succeed (always reversedListOfRootBlocks)
--   ]


parseLevel : String -> Maybe Level
parseLevel levelString =
  ParseValid
      ( Version4
        { attemptOrder = pushAttemptNormal
        , shed = Shed False
        , innerPush = InnerPush False
        , drawStyle = DrawStyleGrid
        , customLevelMusic = -1
        , customLevelPalette = -1
        , level = []
        }
      )
      levelString
  |>discardStartingWhiteSpace
  |>parseLandmarkCaseInsensitive "version 4" -- no word boundary :P
  |>parseLoopWhile
      skipToNextRealData -- delimeter handling
      ( parseLandmarkCaseInsensitive "#" ) -- End of headers
      ( parseOneOf -- checking each header
          [ parseAttemptOrder
          , parseWrappedBoolean "shed" Shed
          , parseWrappedBoolean "inner_push" InnerPush
          , parseDrawStyle
          , parseCustomLevelMusic
          , parseCustomLevelPalette
          ]
      )
  |>discardStartingWhiteSpace
  |>ParseRootBlocks
  |>parseStripper




-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { levelText : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing, Cmd.none )



-- UPDATE


type Msg
  = LevelFindFile
  | LevelSelected File
  | LevelLoaded String
  | LevelSave String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LevelFindFile ->
      ( model
      , Select.file ["text/plain"] LevelSelected
      )

    LevelSelected file ->
      ( model
      , Task.perform LevelLoaded (File.toString file)
      )

    LevelLoaded levelString ->
      ( { model | levelText = Just levelString }
      , Cmd.none
      )

    LevelSave levelString ->
      ( model
      , Download.string "download.txt" "text/plain" levelString
      )


-- VIEW


view : Model -> Html Msg
view model =
  let
    _ =
      (ParseValid () "     q  NaN")
      |>Debug.log "view input"
      |>parseZeroOne Shed
      |>Debug.log "view output"
  in
    case model.levelText of
      Nothing ->
        Html.div []
          [ Html.button [ onClick LevelFindFile ] [ Html.text "Load Level File" ]
          , Html.p [ Attributes.style "white-space" "pre" ]
            [ Html.text "Hallo!"
            ]
          ]

      Just levelText ->
        Html.div []
          [ Html.button [ onClick <| LevelSave levelText ] [ Html.text "Save Level File" ]
          , Html.p [ Attributes.style "white-space" "pre" ] [ Html.text levelText ]
          ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none