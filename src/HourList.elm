module HourList where

import Html exposing (button, div, text, Attribute, input, p, h1, hr)
import Html.Events exposing (onClick)

import HourEditor

type alias ID = Int

type alias Model =
    { editors : List ( ID, HourEditor.Model )
    , nextID : ID
    }

init =
  { editors = [(0, HourEditor.init)]
  , nextID = 1
  }

type Action
    = Insert
    | Remove
    | Modify ID HourEditor.Action

update action model =
  case action of
    Insert ->
      let newEditor = ( model.nextID, HourEditor.init )
          newEditors = model.editors ++ [ newEditor ]
      in
          { model |
              editors = newEditors,
              nextID = model.nextID + 1
          }

    Remove ->
      { model | editors = List.drop 1 model.editors }

    Modify id editorAction ->
      let updateEditor (editorID, editorModel) =
            if editorID == id
                then (editorID, HourEditor.update editorAction editorModel)
                else (editorID, editorModel)
      in
          { model | editors = List.map updateEditor model.editors }

sumAllEditors model =
  List.sum
    (List.map
      (\(_, editorModel) -> HourEditor.calculateDuration editorModel)
      model.editors)

view address model =
  let editors = List.map (viewHourEditor address) model.editors
      remove = button [ onClick address Remove ] [ text "Remove" ]
      insert = button [ onClick address Insert ] [ text "Add" ]
  in
      div []
        [ h1 [] [text "Tunnit"]
        , div [] ([remove, insert] ++ editors)
        , hr [] []
        , div [] [text (toString (sumAllEditors model))]
        ]

viewHourEditor address (id, model) =
  HourEditor.view (Signal.forwardTo address (Modify id)) model
