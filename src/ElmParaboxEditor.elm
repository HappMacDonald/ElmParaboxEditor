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
import Position as Position exposing (Position)
import OrderedSequence exposing (OrderedSequence)
import HSVColor exposing (HSVColor)
import List
-- import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import Tuple

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


-- !!!!
-- -- get this to parse stem via String.startsWith 😵
-- getPushAttemptFromString : String -> Maybe PushAttempt
-- getPushAttemptFromString  =

--   case pushAttempt of
--     Push -> "push"
--     Enter -> "enter"
--     Eat -> "eat"
--     Possess -> "Possess"


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


type FloorButtonType = FloorButton | FloorPlayerButton | FloorInfo String
getFloorButtonTypeString : FloorButtonType -> String
getFloorButtonTypeString floorButtonType =
  case floorButtonType of
    FloorButton -> "Button"
    FloorPlayerButton -> "PlayerButton"
    FloorInfo text -> "Info " ++ text


type PlayerFlag = PlayerFlag Boolean
type PossessableFlag = PossessableFlag Boolean
type Shed = Shed Boolean
type InnerPush = InnerPush Boolean
type FlipHorizontal = FlipHorizontal Boolean


type alias PlayerOptions =
    { player : PlayerFlag
    , possessable : PossessableFlag
    , playerorder : Integer
    }

playerOptionsNone =
  PlayerOptions
    (PlayerFlag False)
    (PossessableFlag False)
    0


type RootBlock =
    RootBlock -- No relevant x/y information
    { id: Integer
    , size: Size
    , color: HSVColor
    , zoomFactor: Float
    -- Presently not allowing root block to be filled with walls
    , playerOptions: PlayerOptions
    , flipHorizontal: FlipHorizontal
    , specialEffect: Integer
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
    , flipHorizontal: FlipHorizontal
    , specialEffect: Integer
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
    , flipHorizontal: FlipHorizontal
    , specialEffect: Integer
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
    , flipHorizontal: FlipHorizontal
    , specialEffect: Integer
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
    , flipHorizontal: FlipHorizontal
    , specialEffect: Integer
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

-- A list of error strings in reverse chrono order
type alias ParseErrors = List String

-- wrapper combinging data already parsed with text remaining to parse next.
type ParseValid payload = ParseValid payload String
type alias ParseResult payload = Result ParseErrors (ParseValid payload)

parseChompAndWrap : String -> Regex -> payload -> ParseResult payload
parseChompAndWrap stringLeftToParse regexToDelete payload =
  Ok
  ( ParseValid
      payload
      ( stringLeftToParse
      |>Regex.replace regexToDelete (always "")
      )
  )


discardWhiteSpace : ParseValid payload -> ParseResult payload
discardWhiteSpace (ParseValid payload input) =
  String.trimLeft input
  |>ParseValid payload
  |>Ok -- ParseResult


-- Only skips whitespace or explicit comments until it gets to
-- data on a real data line.
discardStartingWhiteSpace : ParseValid payload -> ParseResult payload
discardStartingWhiteSpace (ParseValid payload input) =
  let
    regexNoDataThisLine =
      Regex.fromString "^(@|\\n)"
      |>Maybe.withDefault Regex.never

    trimmedInput = String.trimLeft input
    -- _ = Debug.log "discardStartingWhiteSpace" payload
  in
    if trimmedInput |> Regex.contains regexNoDataThisLine
    then
      Ok (ParseValid payload input)
      |>Result.andThen discardExplicitComments
      |>Result.andThen discardStartingWhiteSpace -- recurse on following line

    else
      Ok (ParseValid payload trimmedInput)


discardImplicitComments : ParseValid payload -> ParseResult payload
discardImplicitComments (ParseValid payload input) =
  let
    implicitCommentsRegex : Regex
    implicitCommentsRegex =
      Regex.fromString "^.*(?:\\n|$)"
      |>Maybe.withDefault Regex.never
  in
    Regex.replace implicitCommentsRegex (always "") input
    |>ParseValid payload
    |>Ok -- ParseResult


-- Skips implicit comments on current line,
-- and then any explicit comments or whitespace-only lines
-- AND start-of-line whitespace until it gets back to data on a real data line.
skipToNextRealData : ParseResult payload -> ParseResult payload
skipToNextRealData parseInput =
  parseInput
  |>Result.andThen discardImplicitComments
  |>Result.andThen discardStartingWhiteSpace


discardExplicitComments : ParseValid payload -> ParseResult payload
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
        |>Ok -- has to be on a new line to allow previous line to curry
        |>Result.andThen discardImplicitComments -- ParseResult
      else
        input -- no match
        |>ParseValid payload
        |>Ok -- ParseResult


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


parseZeroOne :
  (Boolean -> newPayloadType)
  -> ParseResult oldPayload
  -> ParseResult (oldPayload, newPayloadType)
parseZeroOne monadWrapper parseInput =
  let
    zeroOneStringToMaybeBoolean : String -> Maybe Boolean
    zeroOneStringToMaybeBoolean zeroOneString =
      case zeroOneString of
        "0" -> Just False
        "1" -> Just True
        _ -> Nothing
  in
    parseInput
    |>Result.andThen
        ( parseThing
            zeroOneBooleanRegexString
            zeroOneStringToMaybeBoolean
        )
    |>parseResultMap
        (\(oldPayload, boolean) -> (oldPayload, monadWrapper boolean) )


-- Does a non-result operation to the payload of a ParseResult,
-- returns an identical ParseResult but with Ok the payload changed.
-- Namely, remaining parse text gets carried smoothly along.
parseResultMap :
  (oldPayload -> newPayload)
  -> ParseResult oldPayload
  -> ParseResult newPayload
parseResultMap lambda parseInput =
  case parseInput of
    Err errors -> Err errors
    Ok (ParseValid payload remainder) ->
      Ok (ParseValid (lambda payload) remainder)


-- I'm leaning toward "won't need this one" due to one above..
-- .. but leaving unfinished bit here in case I waffle back again.
-- ParseResultPayloadCombine : ((a, b) -> c) -> ParseResult (a, b) -> ParseResult c
-- ParseResultPayloadCombine combiner input =



-- This kills any possible remaining parse text metadata btw, lol
-- Might make more strict later and fail on non-empty text *shrugs*
parseStripper : ParseResult payload -> Result ParseErrors payload
parseStripper parseInput =
  case parseInput of
    Ok (ParseValid payload _) -> Ok payload
    Err errors -> Err errors


type CaseSensitivity =
  CaseSensitive
  | CaseInsensitive


-- checks for substring
-- `toMatch` input gets anchored ^ on left for convenience,
-- but is otherwise already considered a regex string.
parseLandmark :
  CaseSensitivity
  -> String
  -> ParseValid oldPayload
  -> ParseResult oldPayload
parseLandmark caseSensitivity toMatch (ParseValid oldPayload toParse) =
  let
    regex =
      Regex.fromStringWith
        { caseInsensitive =
            (caseSensitivity == CaseInsensitive)
        , multiline = False
        }
        ("^"++ toMatch)
      |>Maybe.withDefault Regex.never

  in
    if toParse |> Regex.contains regex
      then
        let
          parsed =
            toParse |> Regex.replace regex (always "")
        in
          Ok (ParseValid oldPayload parsed)
      else
        Err ["Parsing failed at `"++ toMatch ++"`"]


parseIgnore : ParseResult (samePayload, a) -> ParseResult samePayload
parseIgnore parseInput =
  parseInput
  |>parseResultMap (\(oldPayload, _) -> oldPayload)


parseInteger : ParseResult oldPayload -> ParseResult (oldPayload, Integer)
parseInteger parseInput =
    parseInput
    |>Result.andThen
        (parseThing integerRegexString String.toInt)


parseAndIgnoreInteger : ParseResult samePayload -> ParseResult samePayload
parseAndIgnoreInteger parseInput =
    parseInput
    |>parseInteger
    |>parseIgnore


parseFloat : ParseResult oldPayload -> ParseResult (oldPayload, Float)
parseFloat parseInput =
  parseInput
  |>Result.andThen
      (parseThing floatRegexString stringToFloatIncludingSignals)


parseThing :
  String
  -> (String -> Maybe newPayload)
  -> ParseValid oldPayload
  -> ParseResult (oldPayload, newPayload)
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
          -- ParseResult
          [] ->
            Err
              [ "Regex parse failed looking for `"
              ++ regexString
              ++"`: submatches empty list"
              ]

          -- ParseResult
          Nothing :: _ ->
            Err
              [ "Regex parse failed looking for `"
              ++ regexString
              ++"`: submatch was `Nothing`"
              ]

          Just rawMatchString :: _ ->
            case toThingLambda rawMatchString of
              -- ParseResult
              Nothing ->
                Err
                  [ "Regex parse found match, did not fit type for `"
                  ++ regexString
                  ++"`"
                  ]

              Just matchedResult ->
                parseChompAndWrap
                  input regex (payload, matchedResult)
      -- ParseResult
      _ -> Err ["parseThing match was not a 'Match' type? Weird."]


parseOneOf :
  List (ParseValid oldPayload -> ParseResult newPayload)
  -> ParseValid oldPayload
  -> ParseResult (oldPayload, newPayload)
parseOneOf thingsToTry (ParseValid oldPayload oldParseString) =
  case thingsToTry of
    -- all things to try have failed. D:
    [] -> Err ["parseOneOf ran out of things to try"]

    firstThingToTry :: restOfThingsToTry ->
      let
        firstNewResult =
          firstThingToTry (ParseValid oldPayload oldParseString)
      in
        case firstNewResult of
          Err _ -> -- try remainder of things.
            parseOneOf
              restOfThingsToTry
              (ParseValid oldPayload oldParseString)

          Ok (ParseValid newPayload newParseString) ->
            Ok
              ( ParseValid
                (oldPayload, newPayload)
                newParseString
              )


-- returns "Ok payload" if out of non-whitespace data.
-- returns "Err _" if there is non-whitespace data left.
-- returns payload intact, even w/ whitespace, when there is data left to process.
notEndOfFile : ParseValid samePayload -> ParseResult samePayload
notEndOfFile (ParseValid payload toParse) =
  let
    regex : Regex
    regex =
      "^\\s*$" -- entire string consists only of whitespace
      |>Regex.fromString
      |>Maybe.withDefault Regex.never

  in
    if toParse |> Regex.contains regex
    then Ok (ParseValid payload toParse)
    else
      Err
        [ "You shouldn't be reading this"
        , "but there is still more stuff to parse."
        ]




parseLoopWhile :
  (ParseValid payload -> ParseResult payload)
  -> (ParseValid payload -> ParseResult eofGarbage)
  -> (ParseValid payload -> ParseResult payload)
  -> ParseValid payload
  -> ParseResult payload
parseLoopWhile delimiterHandler endMarker parserToRepeat parseInput =
  let
    parseStep1 =
      parseInput
      |>delimiterHandler

    parseEndCheck =
      parseStep1
      |>Result.andThen endMarker
  in
  case parseEndCheck of
    Err _ -> -- begin looping against ParseResultStep1
      let
        errOkLoopedParse =
          parseStep1
          |>Result.andThen parserToRepeat
      in
        case errOkLoopedParse of
          Err errors -> Err errors -- failing a looped parse step ends the whole loop
          Ok loopedParse ->
            loopedParse
            |>parseLoopWhile
                delimiterHandler
                endMarker
                parserToRepeat
    Ok _ ->
      parseStep1 -- finish with this output becaus endcheck ruins whitespace


parseAlways :
  replacementPayload
  -> ParseValid oldPayload
  -> ParseResult replacementPayload
parseAlways replacementPayload (ParseValid _ stringToParse) =
  Ok (ParseValid replacementPayload stringToParse)


parseSize :
  ParseResult RootBlock
  -> ParseResult RootBlock
parseSize oldPayload =
  let
    junk =
      oldPayload
      |>parseInteger
      |>parseInteger
  in
    case junk of
      Err errors -> Err errors

      Ok
        ( ParseValid
          ( ( (RootBlock blockNeedsSize)
            , height
            )
          , width
          )
          parseOutputString
        ) ->
          let
            maybeSize = Size.set width height
          in
            case maybeSize of
              Size.Negative -> Err ["parseSize called on a negative size"]
              Size.Valid size ->
                ( Ok
                  ( ParseValid
                    ( RootBlock
                      { blockNeedsSize | size = size }
                    )
                    parseOutputString
                  )
                )


parsePlayerOptions :
  ParseResult RootBlock
  -> ParseResult RootBlock
parsePlayerOptions inputPayload =
  let
    junk =
      inputPayload
      |>parseZeroOne PlayerFlag
      |>(parseZeroOne PossessableFlag)
      |>parseInteger -- playerorder
  in
    case junk of
      Err errors -> Err errors
      Ok
        ( ParseValid
          ( ( ( (RootBlock blockNeedsPlayerOptions)
              , player -- PlayerFlag
              )
              , possessable -- PossessableFlag
            )
            , playerOrder -- Integer
          ) parseOutputString
        ) ->
        ( Ok
          ( ParseValid
            ( RootBlock
              { blockNeedsPlayerOptions
              | playerOptions =
                  PlayerOptions player possessable playerOrder
              }
            )
            parseOutputString
          )
        )


parseHSVColor :
  ParseResult RootBlock
  -> ParseResult RootBlock
parseHSVColor inputPayload =
  let
    junk =
      inputPayload
      |>parseFloat
      |>parseFloat
      |>parseFloat
  in
    case junk of
      Err errors -> Err errors
      Ok
        ( ParseValid -- <<< this is a long pattern match vvv
          ( ( ( (RootBlock blockRecordNeedsColor)
              , hue
              )
            , saturation
            )
          , value
          )
          parseOutputString
        ) ->
          ( Ok
            ( ParseValid
              ( RootBlock
                { blockRecordNeedsColor
                | color = HSVColor.fromHSV1 hue saturation value
                }
              )
              parseOutputString
            )
          )


parseCustomLevelMusic : ParseValid Level -> ParseResult Level
parseCustomLevelMusic (ParseValid inputLevel stringToParse) =
  (ParseValid inputLevel stringToParse)
  |>parseLandmark CaseSensitive "custom_level_palette "
  |>parseInteger
  |>(\parseResult ->
        case parseResult of
          Err errors -> Err errors
          Ok (ParseValid ((Version4 levelRecords), customLevelMusic) remainingStringToParse) ->
            Ok
            ( ParseValid
              ( Version4
                { levelRecords
                | customLevelMusic = customLevelMusic
                }
              )
              remainingStringToParse
            )
    )


parseCustomLevelPalette : ParseValid Level -> ParseResult Level
parseCustomLevelPalette (ParseValid inputLevel stringToParse) =
  (ParseValid inputLevel stringToParse)
  |>parseLandmark CaseSensitive "custom_level_palette "
  |>parseInteger
  |>parseResultMap
      (\( (Version4 levelRecords), customLevelPalette ) ->
        ( ( Version4
            { levelRecords
            | customLevelPalette = customLevelPalette
            }
          )
        )
      )


parseDrawStyle : ParseValid Level -> ParseResult Level
parseDrawStyle (ParseValid inputLevel stringToParse) =
  let
    (Version4 headers) = inputLevel
    checkCommand =
      Ok (ParseValid inputLevel stringToParse)
      |>Result.andThen
          ( parseLandmark CaseSensitive "draw_style " )
  in
    case checkCommand of
      Err errors -> Err errors
      Ok argumentsToParse ->
        argumentsToParse
        |>parseOneOf
            [ ( parseAlways DrawStyleTUI )
            >>( Result.andThen
                ( parseLandmark
                    CaseSensitive
                    "tui"
                )
              )
            , ( parseAlways DrawStyleGrid )
            >>( Result.andThen
                ( parseLandmark
                    CaseSensitive
                    "grid"
                )
              )
            , ( parseAlways DrawStyleOldStyle )
            >>( Result.andThen
                ( parseLandmark
                    CaseSensitive
                    "oldstyle"
                )
              )
            ]
        |>Result.withDefault (ParseValid (inputLevel, DrawStyleGrid) "")
        |>(\(ParseValid (_, newDrawStyle) _) ->
              Ok
                ( ParseValid
                  ( Version4
                    { headers
                    | drawStyle = newDrawStyle
                    }
                  )
                  stringToParse
                )
          )
        |>Result.andThen
            discardImplicitComments -- discard already processed header line


-- Current problem:
-- Aside from types matching after call to `Tuple.second`,
-- I've got to figure out how this recursion really should bottom out.
parseAttemptOrderArguments :
  ParseResult (OrderedSequence PushAttempt)
  -> OrderedSequence PushAttempt
parseAttemptOrderArguments parseAttemptStack =
  let
    parseResultFirstArgument =
      parseAttemptStack
      |>Result.andThen
          ( parseOneOf
              [ ( parseAlways Enter )
              >>( Result.andThen
                    ( parseLandmark
                        CaseSensitive
                        "enter[^,]*(?:,|$)"
                    )
                )
              , ( parseAlways Eat )
              >>( Result.andThen
                    ( parseLandmark
                        CaseSensitive
                        "eat[^,]*(?:,|$)"
                    )
                )
              , ( parseAlways Push )
              >>( Result.andThen
                    ( parseLandmark
                        CaseSensitive
                        "push[^,]*(?:,|$)"
                    )
                )
              , ( parseAlways Possess )
              >>( Result.andThen
                    ( parseLandmark
                        CaseSensitive
                        "possess[^,]*(?:,|$)"
                    )
                )
              ]
          )
  in
    case parseAttemptStack of
      Err _ -> pushAttemptNormal

      (Ok (ParseValid attemptStack _)) ->
        case parseResultFirstArgument of
          Err _ ->
            attemptStack

          Ok (ParseValid (_, attemptOrderValue) restOfStringToParse) ->
            ( Ok
              ( ParseValid
                ( OrderedSequence.cons
                    attemptOrderValue
                    attemptStack
                )
                restOfStringToParse
              )
            )
            |>parseAttemptOrderArguments



parseAttemptOrder : ParseValid Level -> ParseResult Level
parseAttemptOrder inputParseLevel =
  let
    (ParseValid inputLevel inputStringToParse) = inputParseLevel
    (Version4 headers) = inputLevel
  in
    case
      inputParseLevel
      -- does NOT tolerate preceding whitespace.
      -- or anything but single space trailing, for that matter.
      |>parseLandmark CaseSensitive "attempt_order "
    of
      Err errors -> Err errors -- fail if can't find header token at all
      Ok (ParseValid _ remainingStringToParse) -> -- found, so do not also need unaltered copy of payload.
        Ok
          ( ParseValid
            ( Version4
              { headers
              | attemptOrder =
                  (Ok (ParseValid OrderedSequence.empty remainingStringToParse))
                  |>parseAttemptOrderArguments
                  -- returns `OrderedSequence PushAttempt` directly
              }
            )
            inputStringToParse
          )
        |>Result.andThen
            discardImplicitComments -- discard already processed header line


parseRootBlocks : ParseValid Level -> ParseResult Level
parseRootBlocks (ParseValid (Version4 inputLevel) inputStringToParse) =
  let
    -- Curried rootblock template to clone.
    -- Ok add remaining string to parse! :D
    rootBlockTemplate : String -> ParseValid RootBlock
    rootBlockTemplate =
      ParseValid
      ( RootBlock
        { id = 0
        , size = Size.degenerate
        , color = HSVColor.black
        , zoomFactor = 1
        , playerOptions = playerOptionsNone
        , flipHorizontal = FlipHorizontal False
        , specialEffect = 0
        , children = [] -- parseChildBlocks 1 inputLevel
        }
      )
      -- stringLeftToParse not defined here,
      -- it will be obtained later via curry.
  in
    ParseValid [] inputStringToParse
    |>parseLoopWhile
        Ok -- Wrap ParseValid into ParseResult
        notEndOfFile
        (\(ParseValid accumulatorRootBlocks accumulatorStringToParse) ->
            ( rootBlockTemplate accumulatorStringToParse )
            |>parseLandmark CaseInsensitive "Block "
            |>parseAndIgnoreInteger -- x coordinate not used for root blocks
            |>parseAndIgnoreInteger -- y coordinate not used for root blocks
            |>parseInteger -- id
            |>parseResultMap
                (\((RootBlock rootBlockNeedsId), id) ->
                    RootBlock { rootBlockNeedsId | id = id }
                )
            |>parseSize -- width, height
            |>parseHSVColor
            |>parseFloat -- zoomFactor
            |>parseResultMap
                (\((RootBlock rootBlockNeedszoomFactor), zoomFactor) ->
                    RootBlock { rootBlockNeedszoomFactor | zoomFactor = zoomFactor }
                )
            |>parseAndIgnoreInteger -- fillWithWalls
            |>parsePlayerOptions
            |>parseZeroOne FlipHorizontal -- flipHorizontal
            |>parseResultMap
                (\((RootBlock rootBlockNeedsflipHorizontal), flipHorizontal) ->
                    RootBlock
                    { rootBlockNeedsflipHorizontal
                    | flipHorizontal = flipHorizontal
                    }
                )
            |>parseInteger -- specialEffect
            |>parseResultMap
                (\((RootBlock rootBlockNeedsspecialEffect), specialEffect) ->
                    RootBlock { rootBlockNeedsspecialEffect | specialEffect = specialEffect }
                )
            -- |>parseChildBlocks
            -- |>Result.andThen
            --     (\(rootBlockNeedsChildren, children) ->
            --         { rootBlockNeedsChildren | children = children }
            --     )
            |>parseResultMap -- ball this new rootBlock up with the growing list
                (\rootBlock ->
                    rootBlock :: accumulatorRootBlocks
                )
        )
    |>parseResultMap
        (\allRootBlocks ->
            Version4 { inputLevel | level = allRootBlocks }
        )


-- parseChildBlocks :
-- parseChildBlocks ?? =
--   ??
--   |>parseLoopWhile
--       ??delimiterHandler
--       ??endMarker (ends on too-little indentation)
--       ( parseOneOf
--         [ parseChildBlock
--         , parseReference
--         , parseWall
--         , parseFloor
--         ]
--       )




parseLevelFromString : String -> ParseResult Level
parseLevelFromString levelString =
  Ok
    ( ParseValid
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
    )
  |>Result.andThen discardStartingWhiteSpace
  |>Result.andThen (parseLandmark CaseInsensitive "version 4") -- no word boundary :P
  |>Result.andThen
      ( parseLoopWhile
          Ok -- delimeter handling (monad wrapping identity)
          ( parseLandmark CaseSensitive "#" ) -- End of headers
          ( parseOneOf -- checking each header
              [ parseAttemptOrder
              , parseLandmark CaseSensitive "shed"
                >>( parseZeroOne Shed )
                >>( parseResultMap
                      (\(Version4 level, shed) ->
                          Version4 { level | shed = shed }
                      )
                  )
              , parseLandmark CaseSensitive "inner_push"
                >>( parseZeroOne InnerPush )
                >>( parseResultMap
                      (\(Version4 level, innerPush) ->
                          Version4 { level | innerPush = innerPush }
                      )
                  )
              , parseDrawStyle
              , parseCustomLevelMusic
              , parseCustomLevelPalette
              -- If all else fails, whole line is garbage move on.
              , discardImplicitComments
              ]
            >>(parseResultMap Tuple.second)
          )
      )
  |>Result.andThen discardImplicitComments
  |>Result.andThen discardStartingWhiteSpace
  -- |>Result.andThen parseRootBlocks




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


type alias Model = ParseResult Level

init : () -> (Model, Cmd Msg)
init _ =
  ( Err ["Level not yet loaded"], Cmd.none )



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
      ( Debug.log "Level" (parseLevelFromString levelString)
      , Cmd.none
      )

    LevelSave levelString ->
      ( model
      , Download.string "download.txt" "text/plain" levelString
      )


-- VIEW


renderString : String -> Html Msg
renderString string =
  Html.p []
    [ Html.text string ]



view : Model -> Html Msg
view model =
  case model of
    Err errors ->
      Html.div []
        [ Html.button [ onClick LevelFindFile ] [ Html.text "Load Level File" ]
        , Html.div [ Attributes.style "white-space" "pre" ]
          ( List.map renderString errors )
        ]

    Ok level ->
      let
        regexReinflateNewlines =
          Regex.fromString "[,(){}\\[\\]]|\\\\n"
          |>Maybe.withDefault Regex.never

        debugStrings =
          Debug.toString level
          |>Regex.split regexReinflateNewlines
          |>Debug.log "debugStrings"
      in
      Html.div []
        [ Html.button [ onClick <| LevelSave "Save TBI dawg" ] [ Html.text "Save Level File" ]
        , Html.code [] -- [ Attributes.style "white-space" "pre" ]
          ( ( "Level loaded: "
            ::debugStrings
            )
            |>List.map (\string -> Html.p [] [Html.text string] )
          )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none