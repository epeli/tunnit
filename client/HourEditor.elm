module HourEditor where

import Html exposing (Html, button, div, text, Attribute, input, p, span)
import Html.Attributes exposing (style, value, disabled)
import Html.Events exposing (onClick, on, targetValue)
import String
import Date
import Effects exposing (Effects, Never)
import Http
import Task
import Json.Decode
import Json.Encode


-- MODEL

type alias Inputs =
  { start : String
  , end : String
  , duration : String
  }

type alias Model =
  { id : Int
  , inputs : Inputs
  , savedInputs : Inputs
  , saveStatus : String
  }

init : Int -> (Model, Effects Action)
init id =
  ( initModel id
  , Effects.none
  )

initInputs =
  { start = ""
  , end = ""
  , duration = ""
  }

initModel id =
  { id = id
  , inputs = initInputs
  , savedInputs = initInputs
  , saveStatus = "not saved"
  }

-- UPDATE

type Action
  = UpdateStartTime String
  | UpdateEndTime String
  | UpdateDuration String
  | Reset
  | Save
  | SaveResult (Maybe String)


decodeRes =
  Json.Decode.at ["status"] Json.Decode.string

saveEditor model =
  let
    body = Http.multipart
      [ Http.stringData "start" model.inputs.start
      , Http.stringData "end" model.inputs.end
      ]
  in
    Http.post decodeRes (Http.url ("/row/" ++ (toString model.id)) []) body
      |> Task.toMaybe
      |> Task.map SaveResult
      |> Effects.task


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
  UpdateStartTime t ->
    let
        { inputs } = model
        newInputs = { inputs | start = maxToFive (autoInsertColon t) }
    in
      ( { model | inputs = newInputs }
      , Effects.none
      )

  UpdateEndTime t ->
    let
        { inputs } = model
        newInputs = { inputs | end = maxToFive (autoInsertColon t) }
    in
      ( { model | inputs = newInputs }
      , Effects.none
      )

  UpdateDuration t ->
    let
        { inputs } = model
        newInputs = { inputs | duration = t }
    in
      ( { model | inputs = newInputs }
      , Effects.none
      )

  Reset ->
    ( { model | inputs = model.savedInputs }
    , Effects.none
    )

  Save ->
    ( { model | saveStatus = "saving..." }
    , saveEditor model
    )

  SaveResult maybeOk ->
    (updateFromSaveResult maybeOk model, Effects.none)


updateFromSaveResult maybeOk model =
  case maybeOk of
  Just s ->
    { model | saveStatus = s
    , savedInputs = model.inputs
    }

  Nothing ->
    { model | saveStatus = "error ?" }


hasUnsavedChanges model = model.inputs /= model.savedInputs


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

calculateDuration inputs =
  errToZero (
    if inputs.start /= "" && inputs.end /= "" then
      diffDates inputs.start inputs.end

    else if inputs.duration /= "" then
      String.toFloat inputs.duration

    else
      Ok 0
  )



isDurationDisabled inputs =
  inputs.start /= "" || inputs.end /= ""

isStartEndDisabled inputs =
  inputs.duration /= ""

inputStyle parser value =
  if value == "" then
    style []
  else
    case (parser value) of
      Ok _ ->
        style []

      Err _ ->
        style [ ("background-color", "pink") ]


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ p [ ] [ text (toString (calculateDuration model.inputs)) ]
    , span [] [ text (toString model.id) ]
    , input
      [ disabled (isStartEndDisabled model.inputs)
      , inputStyle parseDate model.inputs.start
      , value model.inputs.start
      , on "input" targetValue (sendUpdate UpdateStartTime address)
      ] []
    , input
      [ disabled (isStartEndDisabled model.inputs)
      , inputStyle parseDate model.inputs.end
      , value model.inputs.end
      , on "input" targetValue (sendUpdate UpdateEndTime address)
      ] []
    , input
      [ disabled (isDurationDisabled model.inputs)
      , inputStyle String.toFloat model.inputs.duration
      , value model.inputs.duration
      , on "input" targetValue (sendUpdate UpdateDuration address)
      ] []
    , button [ onClick address Reset ] [ text "reset" ]
    , button [ onClick address Save ] [ text ("save" ++ (if hasUnsavedChanges model then "*" else "")) ]
    , p [] [ text model.saveStatus ]
    ]


