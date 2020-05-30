# _**A CODE WITH NO PURPOSE AND THE CODE**_
<iframe width="100%" height="600px" style="border:none;background:white;" src="https://macoutreach.rocks/share/9267b362">


module TestCode exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

myShapes model = case model.state of 
                  Home->
                   [ 
                    rectangle 192 128
                    |> filled model.col
                    |> if model.h == model.sizeX && model.v == model.sizeY then notifyTapAt Click else notifyTap Nothing
                    ,
                    roundedRect 80 20 2
                    |> filled green
                    |> move (-70, -60)
                    ,
                    text ("PosX:" ++ String.fromFloat model.sizeX ) 
                    |> size 7
                    |> filled black
                    |> move (-90, -60)
                    ,
                    text ("PosY:" ++ String.fromFloat model.sizeY) 
                    |> size 7 
                    |> filled black
                    |> move (-60, -60)
                    ,
                    roundedRect 90 30 2
                    |> filled red 
                    |> move (70, 60)
                    ,
                    text ("Your Score =" ++ String.fromFloat model.t)
                    |> size 7
                    |> filled white
                    |> move (30, 52.5)
                    ,
                    text(String.fromFloat model.h)
                    |> size 7
                    |> filled white
                    ,
                    text(String.fromFloat model.v)
                    |> size 7
                    |> filled white
                    |> move(10, 0)
                    ,
                    movePacman model.facing model 
                    |> move (model.h, model.v)
                    ,
                    coin model 
                    |> move (model.sizeX, model.sizeY )
                    ] 
                  Win ->
                   [
                    text"Game Over"
                    |> centered
                    |> size 7
                    |> filled (hsl model.time 0 0)
                    ]

pacman model = group [ wedge 5 (clamp 0.75 1 (abs((sin(model.time*10)))))
                |> filled yellow -- you can add more design here!!!
                ,
                oval 1 1.5
                |> filled black 
                |> move (-0.5,3)
              ]

coin model= group [ 
                circle 2 
                |> filled orange
              ]

type State = Home
            | Win

type Msg = Tick Float GetKeyState 
            | Click (Float,Float)
            | Nothing 
            | GoToWin

type Facing = Up 
            | Down
            | Left
            | Right
            | TopLeft
            | TopRight
            | DownLeft
            | DownRight




movePacman facing model = case model.facing of -- these are the directions that pacman can face. For now it only uses the |> rotate, however you might have to use |> mirrorX or mirrorY 
                              Up -> pacman model |> rotate (degrees -90)
                              Down -> pacman model |> rotate (degrees 90)
                              Left -> pacman model |> rotate (degrees 0)
                              Right -> pacman model |> mirrorX
                              TopLeft -> pacman model |> rotate (degrees 45)
                              DownRight -> pacman model |> rotate (degrees 45) |> mirrorX
                              DownLeft -> pacman model |> rotate (degrees -45)
                              TopRight -> pacman model |> rotate (degrees -45) |> mirrorX



update msg model = 
                  case msg of
                    Tick t (_,(nx,ny),_)
                       ->  { model
                           | time = t
                          , h = if toFloat(round(model.h)) > model.sizeX then model.h - 1 else if toFloat(round(model.h)) < model.sizeX then model.h + 1 else model.sizeX
                          , v = if model.v > model.sizeY then model.v - 1 else if model.v < model.sizeY then model.v + 1 else model.sizeY
                          , t = if model.v == model.sizeY && model.h == model.sizeX then model.t + 1 else if abs(model.time) >= 9 then model.t - model.t else model.t
                          , facing = if model.h < model.sizeX && model.v < model.sizeY then TopRight else if model.h < model.sizeX && model.v > model.sizeY then DownRight else if model.h > model.sizeX && model.v > model.sizeY then TopLeft else if model.h > model.sizeX && model.v < model.sizeY then DownLeft else if model.h > model.sizeX then Left  else if model.h < model.sizeX then Right else if model.v < model.sizeY then Up else if model.v > model.sizeY then Down else model.facing
                           }
                    Click (x,y) -> { model | sizeX = toFloat(round(x)), sizeY = toFloat(round(y)), score = model.score + 1, col = blue } 
                    Nothing -> { model | col = red } 
                    GoToWin ->
                      case model.state of 
                          Home -> { model | state = Win}
                          otherwise ->
                            model

init = { time = 0, v = 0, h = 0, value = 0.75, facing = Up, sizeX = 0, sizeY = 0, t = 0, score = 0, col = red, state = Home}



main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot"}


view model =
    collage 192 128 (myShapes model)
    
## There are always ways to have, even during quarantine fun whether it be by Making games or Playing them!
##  **Have Fun**
