module HourList where

import Html exposing (button, div, text, Attribute, input, p, h1, hr)
import Html.Events exposing (onClick)

import HourEditor

type alias ID = Int

type alias Model =
    { editors : List HourEditor.Model
    , nextID : ID
    }

init =
  { editors = [HourEditor.init 0]
  , nextID = 1
  }

type Action
    = Insert
    | Remove
    | Modify ID HourEditor.Action

update action model =
  case action of
    Insert ->
      let newEditor = HourEditor.init model.nextID
          newEditors = model.editors ++ [ newEditor ]
      in
          { model |
              editors = newEditors,
              nextID = model.nextID + 1
          }

    Remove ->
      { model | editors = List.drop 1 model.editors }

    Modify id editorAction ->
      let updateEditor editorModel =
            if editorModel.id == id
                then HourEditor.update editorAction editorModel
                else editorModel
      in
          { model | editors = List.map updateEditor model.editors }

sumAllEditors model =
  List.sum
    (List.map
      (\editorModel -> HourEditor.calculateDuration editorModel)
      model.editors
    )

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

viewHourEditor address model =
  HourEditor.view (Signal.forwardTo address (Modify model.id)) model
