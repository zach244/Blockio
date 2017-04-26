module Blockio exposing(..)
import Html exposing (program)
import Keyboard exposing (..)
import AnimationFrame
import Time exposing (..)
import Collage exposing (..)
import Collision2D
import Color exposing (..)
import Element exposing(..)
import Text exposing(..)
-- import Window






main = program
    { init = (initialModel, Cmd.none),
      view = view,
      subscriptions = subscriptions,
      update = updateWithCommand
    }

type Msg = KeyMsg KeyTypes | Tick Time
type KeyTypes = KeyUp Int | KeyDown Int
type alias Spike =
  {
  base : Int
  , ht : Int
  , x : Int
  }

initialModel = {x = 0, vx = 0,
                y = 0, vy = 0,
                blockioAcceleration = 0}

--door = (Svg.rect [x "0", y "0", width "25", height "25"] [])

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
gravityS = 0.2
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

        --S key
        KeyDown 83 -> fastFall 3

        _ -> identity

run moveSpeed model =
  {model | blockioAcceleration = moveSpeed}

fastFall moveSpeed model =
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

--Blockio's hit box using collision
--rectangle takes centerx, centery, width, height
--centerx = model.x
--centery = model.y
--width = width "given in view's svg dimensions"
--height = height "   "

tick model =
    model
        |> gravity
        |> motion
        |> floor

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
--kill function should be very similar to floor and added to tick
view model =
    toHtml(
      collage 1000 500 [(
        (moveY -225 (moveX model.x (filled (black ) (rect 25 25))))),
         (moveY -250 (filled (black ) (rect 1000 20))),
        -- line is created for the floor
         (moveY 0 (moveX -model.x (filled (black ) (ngon 3 15)))),
        -- alien is created and only moves opposite the tank for now
         ( toForm (centered (fromString ("Press left/right arrows to control tank and the up arrow to shoot"))))
        -- adds text instructions for now
        ]
        )
