module ElmParaboxEditor exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events exposing (onClick)
import Task exposing (Task)
import Size exposing (Size)
import Position exposing (Position)
import OrderedSequence exposing (OrderedSequence)
import HSVColor exposing (HSVColor)
import List

type PushAttempt = Push | Enter | Eat | Possess

getPushAttemptString : PushAttempt -> String
getPushAttemptString pushAttempt =
  case pushAttempt of
    Push -> "push"
    Enter -> "enter"
    Eat -> "eat"
    Possess -> "Possess"

type DrawStyle = TUI | Grid | OldStyle

getDrawStyleString : DrawStyle -> String
getDrawStyleString drawStyle =
  case drawStyle of
    TUI -> "tui"
    Grid -> "grid"
    OldStyle -> "oldstyle"

type FloorButtonType = FloorButton | FloorPlayerButton

getFloorButtonTypeString : FloorButtonType -> String
getFloorButtonTypeString floorButtonType =
  case floorButtonType of
    FloorButton -> "Button"
    FloorPlayerButton -> "PlayerButton"

type alias Boolean = Bool -- Abbreviations are bad.
type alias Integer = Int -- Abbreviations are bad.
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
  { attemptOrder : OrderedSequence (List PushAttempt)
  , shed : Shed
  , innerPush : InnerPush
  , drawStyle : DrawStyle
  , customLevelMusic : Integer
  , customLevelPalette : Integer
  , level : List RootBlock
  }




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
  { csv : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing, Cmd.none )



-- UPDATE


type Msg
  = CsvRequested
  | CsvSelected File
  | CsvLoaded String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CsvRequested ->
      ( model
      , Select.file ["text/csv"] CsvSelected
      )

    CsvSelected file ->
      ( model
      , Task.perform CsvLoaded (File.toString file)
      )

    CsvLoaded content ->
      ( { model | csv = Just content }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  case model.csv of
    Nothing ->
      Html.button [ onClick CsvRequested ] [ Html.text "Load CSV" ]

    Just content ->
      Html.p [ Attributes.style "white-space" "pre" ] [ Html.text content ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none