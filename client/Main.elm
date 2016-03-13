import Effects exposing (Never)
-- import HourList exposing (update, view, init)
import HourEditor exposing (update, view, init)
import StartApp
import Task


app =
  StartApp.start
    { init = init 1
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

