import Html exposing (Html, text, program)
import Keyboard
import Blockio

subscriptions : model -> Sub Msg

subscriptions model =
  Sub.batch
  [
  Keyboard.downs KeyMsg
  ]

type Msg = KeyMsg Keyboard.KeyCode

view : model -> Html Msg
view model = text "Press any key to continue"
--  div []
--    [ [ text "BLOCKIO"]
--    , div [] [text "press enter"]
--    , []
--    ]

update msg model =
  case msg of
     KeyMsg code ->
        Blockio.main

main =
    program {view = view, update = update, subscriptions = subscriptions}
