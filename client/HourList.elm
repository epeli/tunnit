module HourList where

import Html exposing (button, div, text, Attribute, input, p, h1, hr)
import Html.Events exposing (onClick)
import Effects exposing (Effects, Never)

import HourEditor


type alias Model =
  { editors : List HourEditor.Model
  , nextID : Int
  }


init id =
  ( { editors = [HourEditor.initModel id]
    , nextID = id + 1
    }
  , Effects.none
  )

type Action
  = Insert
  | Remove
  | Modify Int HourEditor.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Insert ->
      let
        (newEditor, fx) = HourEditor.init model.nextID
        newEditors = model.editors ++ [ newEditor ]
      in
        ( { model |
            editors = newEditors,
            nextID = model.nextID + 1
          }
        , Effects.map (Modify newEditor.id) fx
        )

    Remove ->
      ( { model | editors = List.drop 1 model.editors }
      , Effects.none
      )

    Modify id editorAction ->
      let
        updateEditor editorModel =
          if editorModel.id == id then
            let
              (newEditor, fx) = HourEditor.update editorAction editorModel
            in
              ( newEditor
              , Effects.map (Modify id) fx
              )
           else
            (editorModel, Effects.none)


        (newEditors, fxList) =
          model.editors
            |> List.map updateEditor
            |> List.unzip

      in
        ( { model | editors = newEditors }
        , Effects.batch fxList
        )


sumAllEditors model =
  model.editors
    |> List.map (\m -> m.inputs)
    |> List.map HourEditor.calculateDuration
    |> List.sum


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    editors = List.map (viewHourEditor address) model.editors
    remove = button [ onClick address Remove ] [ text "Remove" ]
    insert = button [ onClick address Insert ] [ text "Add" ]
  in
    div []
      [ h1 [] [text "Tunnit"]
      , div [] ([remove, insert] ++ editors)
      , hr [] []
      , div [] [text (toString (sumAllEditors model))]
      ]

viewHourEditor address model =
  HourEditor.view (Signal.forwardTo address (Modify model.id)) model
