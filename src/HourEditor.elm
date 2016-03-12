module HourEditor where

import Html exposing (button, div, text, Attribute, input, p, h1)
import Html.Attributes exposing (style, value, disabled)
import Html.Events exposing (onClick, on, targetValue)
import String
import Date


-- MODEL

type alias Model =
  { start : String
  , end : String
  , duration : String
  }

init =
    { start = ""
    , end = ""
    , duration = ""
    }

-- UPDATE

type Action = UpdateStartTime String
            | UpdateEndTime String
            | UpdateDuration String
            | Reset

update action model =
  case action of

    UpdateStartTime t ->
        { model | start = autoInsertColon t }

    UpdateEndTime t ->
        { model | end = t }

    UpdateDuration t ->
        { model | duration = t }

    Reset -> init


mapWithIndex mapper s =
  String.fromList
    (List.map2 mapper ([0..(String.length s)]) (String.toList s))

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

-- VIEW

sendUpdate action address val =
  Signal.message address (action val)


timeToSomeDate t =
  "Sat, 12 Mar 2016 " ++ t ++ ":00 GMT"


parseDate s =
  if s == ""
    then Err "no value to parse"
    else Date.fromString (timeToSomeDate s)

msToHours m = m / 1000 / 60 / 60

diffDates start end =
  case ((parseDate start), (parseDate end)) of
    (Ok s, Ok e) ->
      Ok (msToHours ((Date.toTime e) - (Date.toTime s)))

    (Err s, _) ->
      Err s

    (_, Err s) ->
      Err s


errToZero res =
  case res of
    Ok d -> d
    Err _ -> 0.0

calculateDuration model =
  errToZero (
    if model.start /= "" && model.end /= "" then
      diffDates model.start model.end

    else if model.duration /= "" then
      String.toFloat model.duration

    else
      Ok 0
  )



isDurationDisabled model =
  model.start /= "" || model.end /= ""

isStartEndDisabled model =
  model.duration /= ""

inputStyle parser value =
    if value == "" then
        style []
    else
      case (parser value) of
        Ok _ ->
          style []

        Err _ ->
          style [ ("background-color", "pink") ]

view address model =
  div []
    [ h1 [ ] [ text "Tunnit" ]
    , p [ ] [ text (toString (calculateDuration model)) ]
    , input
      [ disabled (isStartEndDisabled model)
      , inputStyle parseDate model.start
      , value model.start
      , on "input" targetValue (sendUpdate UpdateStartTime address)
      ] []
    , input
      [ disabled (isStartEndDisabled model)
      , inputStyle parseDate model.end
      , value model.end
      , on "input" targetValue (sendUpdate UpdateEndTime address)
      ] []
    , input
      [ disabled (isDurationDisabled model)
      , inputStyle String.toFloat model.duration
      , value model.duration
      , on "input" targetValue (sendUpdate UpdateDuration address)
      ] []
    , button [ onClick address Reset ] [ text "clear" ]
    ]


