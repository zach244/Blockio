import Html exposing (Html, div, text, program)
import Keyboard

type Msg = KeyMsg Keyboard.KeyCode

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [
  Keyboard.downs KeyMsg
  ]

view model =
  div []
    [ [ text "BLOCKIO"]
    , div [] [text "press enter"]
    , []
    ]

type Msg = Enter

update msg model =
  case msg of
    Enter ->
      Blockio

main =
    program {view = view, update = update}
