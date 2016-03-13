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

type alias Model =
  { id : Int
  , start : String
  , end : String
  , duration : String
  , saveStatus : String
  }

init : Int -> (Model, Effects Action)
init id =
    let
        initialModel = initModel id
    in
    ( initialModel
    , Effects.none
    )

initModel id =
  { id = id
  , start = ""
  , end = ""
  , duration = ""
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
        [ Http.stringData "start" model.start
        , Http.stringData "end" model.end
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
        ( { model | start = maxToFive (autoInsertColon t) }
        , Effects.none
        )

    UpdateEndTime t ->
        ( { model | end = maxToFive (autoInsertColon t) }
        , Effects.none
        )

    UpdateDuration t ->
        ( { model | duration = t }
        , Effects.none
        )

    Reset ->
      ( initModel model.id
        , Effects.none
      )

    Save ->
      (model, saveEditor model)

    SaveResult maybeOk ->
      (updateFromSaveResult maybeOk model, Effects.none)


updateFromSaveResult maybeOk model =
  case maybeOk of
    Just s ->
      { model | saveStatus = s }

    Nothing ->
      model




maxToFive s =
  if String.length s > 5 then
    String.slice 0 5 s
  else
    s

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

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ p [ ] [ text (toString (calculateDuration model)) ]
    , span [] [ text (toString model.id) ]
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
    , button [ onClick address Save ] [ text "save" ]
    , p [] [ text model.saveStatus ]
    ]


