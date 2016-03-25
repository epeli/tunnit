module TimeInput where

import Html exposing (Html, span, text, input, p, span, button)
import Html.Attributes exposing (style, value, disabled, href)
import Html.Events exposing (onClick, on, targetValue)
import TaskTutorial exposing (getCurrentTime)
import Effects
import Date
import String
import Signal


type alias Model = String


type Action = SetNow | Update String


padZero = toString >> String.padLeft 2 '0'
formatTimeStampAsClock stamp =
  let
    t = Date.fromTime stamp
  in
    (padZero (Date.hour t)) ++ ":" ++ (padZero (Date.minute t))


autoInsertColon s =
  if not (String.contains ":" s) && String.length s == 3 then
    (String.slice 0 2 s) ++ ":" ++ String.right 1 s

  else
    s

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
  Update s ->
    ( String.slice 0 5 (autoInsertColon s)
    , Effects.none
    )

  SetNow ->
    ( model
    , getCurrentTime
        |> Effects.task
        |> Effects.map formatTimeStampAsClock
        |> Effects.map Update
    )

parseDate s =
  if s == ""
  then Err "no value to parse"
  else Date.fromString (timeToSomeDate s)

sendUpdate action address val =
  Signal.message address (action val)

inputStyle parser value =
  if value == "" then
    style []
  else
    case (parser value) of
      Ok _ ->
        style []

      Err _ ->
        style [ ("background-color", "pink") ]


timeToSomeDate t =
  "Sat, 12 Mar 2016 " ++ t ++ ":00 GMT"


view address model isDisabled =
  span []
    [ input
      [ disabled isDisabled
      , inputStyle parseDate model
      , value model
      , on "input" targetValue (sendUpdate Update address)
      ] []
    , button [ onClick address SetNow ] [ text "now" ]
    ]
