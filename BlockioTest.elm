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


type State =
    NoKey
    |Dead
    |HasKey
    |Win



main = program
    { init = (initialModel, Cmd.none),
      view = view,
      subscriptions = subscriptions,
      update = updateWithCommand
    }

type Msg = KeyMsg KeyTypes | Tick Time
type KeyTypes = KeyUp Int | KeyDown Int
-- type alias Spike =
--   {
--   base : Int
--   , ht : Int
--   , x : Int
--   }

initialModel = {
                blockio = initialBlockio,
                key = initialKey,
                door = initialDoor,
                state = NoKey
              }
initialBlockio = {
                x = 0, vx = 0,
                y = 0, vy = 0,
                blockioAcceleration = 0,
                lives = 3
                }
initialKey = {
    centerX = 500,
    centerY = 0,
    bottomCornorX = -70,
    bottomCornorY = -70,
    topCornorX = -70,
    topCornorY = 70
  }
initialDoor = {
  centerX = -500,
  centerY = 0,
  bottomCornorX = 70,
  bottomCornorY = -70,
  topCornorX = 70,
  topCornorY = 70
  }
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


runSpeed = 10
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
        -- KeyUp 83 ->

        _ -> identity

run moveSpeed model =
  let b = model.blockio in
    {model | blockio = {b | blockioAcceleration = moveSpeed}}

fastFall moveSpeed model =
  let b = model.blockio in
    {model | blockio = {b | vy = (model.blockio.vy - moveSpeed)}}

jump model =
  if model.blockio.vy == 0 then
    let b = model.blockio in
      {model | blockio = {b | vy = jumpSpeed}}
  else
    model

stop condition model =
  if condition model.blockio.blockioAcceleration 0 then
    let b = model.blockio in
      {model | blockio = {b | blockioAcceleration = 0}}
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
        |> kill
        |> getKey
        |> doorCollide
        |> spikeAirCollision
        |> spikeGroundCollision
        |> acceleration
        |> gravity
        |> motion
        |> walls
        |> floor
-- alienHitBox model =

kill model =
  if model.blockio.lives == 0 then
    {model | state = Dead}
  else
    model

gravity model =
  let b = model.blockio in
  {model | blockio = {b | vy = model.blockio.vy - gravityS}}

motion model =
  let b = model.blockio in
    {model | blockio = {b | x = model.blockio.x + model.blockio.vx,
                            y = model.blockio.y + model.blockio.vy
                          }
  }
acceleration model =
  let b = model.blockio in
    {model | blockio = {b | vx = model.blockio.blockioAcceleration}}

floor model =
    if model.blockio.y < 0 then
      let b = model.blockio in
       {model | blockio = {b | y = 0, vy = 0}}
    else
       model

spikeGroundCollision model =
  -- spike 3
  if (model.blockio.x) == 500 && (model.blockio.y) == 0 then
    let b = model.blockio in
      {model | blockio = { b | x = 0,
                              y = 0,
                              lives = (model.blockio.lives - 1)}}


            -- if (400 <= model.blockio.x >= 500) && ()
  else if (model.blockio.x) == 485 && (model.blockio.y) == 0 then
    let b = model.blockio in
      {model | blockio = { b | x = 0,
                              y = 0,
                              lives = (model.blockio.lives - 1)}}

  -- spike 2
  else if (model.blockio.x) == 450 && (model.blockio.y) == 0 then
    let b = model.blockio in
      {model | blockio = { b | x = 0,
                              y = 0,
                              lives = (model.blockio.lives - 1)}}
    -- spike 1
  else if (model.blockio.x) == 435 && (model.blockio.y) == 0 then
    let b = model.blockio in
      {model | blockio = { b | x = 0,
                              y = 0,
                              lives = (model.blockio.lives - 1)}}
  else
    model

spikeAirCollision model =
  if (model.blockio.x - 445) == 5 && (model.blockio.y - 255) == 5 then
    let b = model.blockio in
      {model | blockio = { b | x = 0,
                              y = 0,
                              lives = (model.blockio.lives - 1)}}

  else model

walls model =
  if 1400 <= model.blockio.x then
    let b = model.blockio in
      {model | blockio = {b | x = 1400}}
    else if -1400 >= model.blockio.x then
      let b = model.blockio in
        {model | blockio = {b | x = -1400}}
        else model

doorCollide model =
  if (model.blockio.x == 0 && model.state == HasKey) then
    {model | state = Win}
  else
    model

getKey model =
    if (model.blockio.x == 1400 && model.state == NoKey) then
      {model | state = HasKey}
    else
      model



view model =
  if model.state == NoKey then
    toHtml(
      collage 2000 500 [(
        (moveY (model.blockio.y - 230) (moveX (model.blockio.x - 445) (filled (black ) (rect 25 25))))),
         (moveY -250 (filled (black ) (rect 2000 20))),
        -- line is created for the floor
         (moveY -200 (moveX -950 (toForm (image 100 100 "Door.png")))),
         (moveY -200 (moveX 950 (toForm (image 100 100 "Key.png")))),
         (rotate (degrees 330)(moveY -245 (moveX 0 (filled (black ) (ngon 3 15))))),
         (rotate (degrees 330)(moveY -245 (moveX 30 (filled (black ) (ngon 3 15))))),
         (rotate (degrees 330)(moveY -245 (moveX 60 (filled (black ) (ngon 3 15))))),
         (rotate (degrees 330)(moveY -245 (moveX 90 (filled (black ) (ngon 3 15))))),
        -- alien is created and only moves opposite the tank for now
         ( toForm (centered (fromString ("Press W,A,S,D to control Blockio"))))
        -- adds text instructions for now
        ]
        )
      else if model.state == HasKey then
        toHtml(
          collage 2000 500 [(
            (moveY (model.blockio.y - 230) (moveX (model.blockio.x - 445) (filled (black ) (rect 25 25))))),
             (moveY -250 (filled (black ) (rect 2000 20))),
            -- line is created for the floor
             (moveY -200 (moveX -950 (toForm (image 100 100 "Door.png")))),
             (rotate (degrees 330)(moveY -245 (moveX 0 (filled (black ) (ngon 3 15))))),
             (rotate (degrees 330)(moveY -245 (moveX 30 (filled (black ) (ngon 3 15))))),
             (rotate (degrees 330)(moveY -245 (moveX 60 (filled (black ) (ngon 3 15))))),
             (rotate (degrees 330)(moveY -245 (moveX 90 (filled (black ) (ngon 3 15))))),
            -- alien is created and only moves opposite the tank for now
             ( toForm (centered (fromString ("Press W,A,S,D to control Blockio"))))
            -- adds text instructions for now
            ]
            )
          else if model.state == Win then
            toHtml(
              collage 2000 500 [

                 ( toForm (centered (fromString ("You Won!!! Theres nothing left"))))
                -- adds text instructions for now
                ]
                )
            else
              toHtml(
                collage 2000 500 [

                   ( toForm (centered (fromString ("Game Over!!!"))))
                  -- adds text instructions for now
                  ]
                  )
