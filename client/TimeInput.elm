module TimeInput where

import Html exposing (Html, span, text, input, p, span, a)
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


maxToFive s =
  if String.length s > 5
  then String.slice 0 5 s
  else s

mapWithIndex mapper s =
  String.fromList
  ( List.map2 mapper
      ([0..(String.length s)])
      (String.toList s)
  )

thirdToColon i c = if i == 2 then ':' else c

lastNonColon s =
  let
    last = String.right 1 s
  in
  if last == ":" then
    ""
  else
    last

autoInsertColon s =
  if String.length s == 3 then
    (String.slice 0 2 s) ++ ":" ++ (lastNonColon s)

  else if String.length s > 2 then
    mapWithIndex thirdToColon s

  else
    s

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
  Update s ->
    ( maxToFive (autoInsertColon s)
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
    , a [ href "#", onClick address SetNow ] [ text "now" ]
    ]
