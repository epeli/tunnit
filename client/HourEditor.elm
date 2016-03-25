module HourEditor where

import Html exposing (Html, button, div, text, Attribute, input, p, span)
import Html.Attributes exposing (style, value, disabled, href)
import Html.Events exposing (onClick, on, targetValue)
import String
import Date
import Time
import Effects exposing (Effects, Never)
import Http
import Task
import Json.Decode
import Json.Encode
import TaskTutorial exposing (print, getCurrentTime)
import TimeInput


-- MODEL

type alias Inputs =
  { start : TimeInput.Model
  , end : TimeInput.Model
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
  = UpdateStartTime TimeInput.Action
  | UpdateEndTime TimeInput.Action
  | UpdateDuration String
  | Reset
  | Save
  | SaveResult (Maybe String)
  | SetPrevToStart String


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

  UpdateDuration t ->
    let
        { inputs } = model
        newInputs = { inputs | duration = t }
    in
      ( { model | inputs = newInputs }
      , Effects.none
      )

  UpdateStartTime action ->
    let
        (newStart, fx) = TimeInput.update action model.inputs.start
        { inputs } = model
        newInputs = { inputs | start = newStart }
    in
      ( { model | inputs = newInputs }
      , Effects.map UpdateStartTime fx
      )

  UpdateEndTime action ->
    let
        (newEnd, fx) = TimeInput.update action model.inputs.end
        { inputs } = model
        newInputs = { inputs | end = newEnd }
    in
      ( { model | inputs = newInputs }
      , Effects.map UpdateEndTime fx
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

  SetPrevToStart t ->
    update (UpdateStartTime (TimeInput.Update t)) model


updateFromSaveResult maybeOk model =
  case maybeOk of
  Just s ->
    { model | saveStatus = s
    , savedInputs = model.inputs
    }

  Nothing ->
    { model | saveStatus = "error ?" }


hasUnsavedChanges model = model.inputs /= model.savedInputs



-- VIEW

sendUpdate action address val =
  Signal.message address (action val)


msToHours m = m / 1000 / 60 / 60

diffDates start end =
  case ((TimeInput.parseDate start), (TimeInput.parseDate end)) of
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



view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ p [ ] [ text (toString (calculateDuration model.inputs)) ]

    , span [] [ text (toString model.id) ]

    , TimeInput.view (Signal.forwardTo address UpdateStartTime) model.inputs.start (isStartEndDisabled model.inputs)
    , button [ onClick address (SetPrevToStart "") ] [ text "prev" ]
    , TimeInput.view (Signal.forwardTo address UpdateEndTime) model.inputs.end (isStartEndDisabled model.inputs)

    , input
      [ disabled (isDurationDisabled model.inputs)
      , TimeInput.inputStyle String.toFloat model.inputs.duration
      , value model.inputs.duration
      , on "input" targetValue (sendUpdate UpdateDuration address)
      ] []

    , button [ onClick address Reset ] [ text "reset" ]

    , button [ onClick address Save ] [ text ("save" ++ (if hasUnsavedChanges model then "*" else "")) ]

    , p [] [ text model.saveStatus ]

    ]


