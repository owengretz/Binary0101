-- https://cs1xd3.online/ShowModulePublish?modulePublishId=a7a49b28-682f-4586-b9ac-af2b55d5d589
-- lesson 2 visual:
--   computer switch to show why computers use binary

myShapes model =
  [ switch model.on
  , scroller model.on
  , bit model.on
  ]

switch on =
  [
  rect 75 7
    |> filled white
    |> addOutline (solid 0.5) black
    |> move (-75,0)
  ,
  rect 75 7
    |> filled white
    |> addOutline (solid 0.5) black
    |> move (75,0)
  ,
  rect 75 7
    |> filled white
    |> addOutline (solid 0.5) black
    |> move (40,0)
    |> rotate (if on then 0 else pi/8)
    |> move (-40,0)
  ,
  circle 8
    |> filled white
    |> addOutline (solid 0.5) black
    |> move (-40,0)
  ,
  circle 8
    |> filled white
    |> addOutline (solid 0.5) black
    |> move (40,0)
  ] |> group
    |> move (0,5)

scroller on = 
  group [ roundedRect 12 22 1 
            |> filled black 
            |> move (50, 0)
        , roundedRect 10 20 1 
            |> filled white 
            |> move (50, 0)
        , roundedRect 10 10 1 
            |> filled black 
            |> move (if not on then (50,-5) else (50, 5))
        ] |> move (20,30) |> notifyTap IsOn

bit on = group [ text (if on then "1" else "0") 
                   |> centered
                   |> size 15 
                   |> filled black 
                   |> move (0, -20) ] 

type State = On | Off
type Msg = Tick Float GetKeyState
          | IsOn

type alias Model = { time : Float, on : Bool, state : State }

update msg model = case msg of
                     Tick t _ -> { model | time = t }
                     IsOn -> 
                           case model.state of 
                             On -> { model | on = False, state = Off }
                             Off -> { model | on = True, state = On }

init = { time = 0, on = False, state = Off }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)