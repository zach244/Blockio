module Blockio exposing(..)
import Html exposing (program)
import Keyboard
import AnimationFrame
import Time exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Collision2D



main = program
    { init = (initialModel, Cmd.none),
      view = view,
      subscriptions = subscriptions,
      update = updateWithCommand
    }

type Msg = KeyMsg KeyTypes | Tick Time
type KeyTypes = KeyUp Int | KeyDown Int

initialModel = {x = 0, vx = 0,
                y = 0, vy = 0,
                blockioAcceleration = 0}

subscriptions model =
  Sub.batch
    [ Keyboard.downs (KeyDown >> KeyMsg)
    , Keyboard.ups (KeyUp >> KeyMsg)
    , AnimationFrame.diffs Tick
    ]

updateWithCommand msg model =
  (update msg model, Cmd.none)

update msg model =
  case msg of
    KeyMsg k -> key k model
    Tick time -> tick model

runSpeed = 4
gravityS = 0.5
jumpSpeed = 5

key msg model =
  model
    |> case msg of
      --W key
        KeyDown 87 -> jump

        --A key
        KeyDown 65 -> run -runSpeed
        KeyUp 65 -> stop (<)

        --D key
        KeyDown 68 -> run runSpeed
        KeyUp 68 -> stop (>)

        _ -> identity

run moveSpeed model =
  {model | blockioAcceleration = moveSpeed}

jump model =
  if model.vy == 0 then
    {model | vy = jumpSpeed}
  else
    model

stop condition model =
  if condition model.blockioAcceleration 0 then
    {model | blockioAcceleration = 0}
  else
    model


tick model =
    model
        -- |> collision
        |> gravity
        |> motion
        |> floor
-- collision model =

gravity model =
  {model | vy = model.vy - gravityS}

motion model =
  {model | x = model.x + model.vx,
          y = model.y + model.vy
  }

floor model =
    if model.y < 0 then
       {model | y = 0, vy = 0}
    else
       model

view model =
    svg
      -- List of attributes of SVG node
      [ width "100%", height "100%" -- Scale up to take the full page
      , viewBox "0 0 500 100" -- Numbers used in the drawing are relative to these viewBox dimensions. 0 in the y dimension is the top of the drawing.
      ]
      -- List of children
      [ circle [ cx (toString (model.x + 16))
               , cy (toString (75 - model.y))
               , r "16"] []
      , line [ x1 "0", y1 "95"
             , x2 "500", y2 "95"
             , strokeWidth "8"
             , stroke "black" ] [] ]
