module Blockio exposing(..)
import Html exposing (..)
import Keyboard exposing (..)
import Svg exposing(..)
import AnimationFrame
import Svg.Attributes exposing(..)
import Time exposing(..)




view model =
    svg
      -- List of attributes of SVG node
      [ width "100%", height "100%" -- Scale up to take the full page
      , viewBox "0 0 500 100" -- Numbers used in the drawing are relative to these viewBox dimensions. 0 in the y dimension is the top of the drawing.
      ]
      -- List of children
      [ rect [ ] []
      , line [ x1 "0", y1 "95"
             , x2 "500", y2 "95"
             , strokeWidth "8"
             , stroke "black" ] [] ]
type alias Model = Int

model : Model
model =
  0
update : int -> Model -> Model
update = 42

main  = Html.beginnerProgram{view = view, model = model, update = update}

