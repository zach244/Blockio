import Html exposing (Html, div, text, program)
import Keyboard
Module Blockio

subscriptions : Model -> Sub Msg

subscriptions model =
  Sub.batch
  [
  Keyboard.downs KeyMsg
  ]

type Msg = KeyMsg Keyboard.KeyCode

view : Model -> Html Msg
view model =
  div []
    [ [ text "BLOCKIO"]
    , div [] [text "press enter"]
    , []
    ]

update msg model =
  case msg of
     KeyMsg code ->
      Blockio

main =
    program {view = view, update = update}
