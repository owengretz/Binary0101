-- https://cs1xd3.online/ShowModulePublish?modulePublishId=9c431711-76b3-402f-baac-55f8b6b39c92
-- lesson 3 visual:
--   light bulb to represent binary & help understand binary visually

myShapes model =
  [   light model.on
    , scroller model.on
    , digit model.on
  ]

light on = group [ 
                oval 15 17 |> filled (rgb 215 204 182)
              , oval 15 17 |> if on
                              then filled yellow
                              else outlined (solid 1) yellow 
              , roundedRect 9 2 1 |> filled charcoal |> move (0, 8)
              , roundedRect 8 2 1 |> filled charcoal |> move (0, 10.5)
              , roundedRect 7 2 1 |> filled charcoal |> move (0, 13)
              , rect 1 50 |> filled charcoal |> move (0, 39)
              ] |> notifyTap IsOn
              
scroller on = group [ roundedRect 12 22 1 |> filled black |> move (50, 0)
                 , roundedRect 10 20 1 |> filled white |> move (50, 0)
                 , roundedRect 10 10 1 |> filled black 
                                       |> move (if on then (50,5) else (50,-5))
                                       ] |> notifyTap IsOn

digit on = group [text (if on then "1" else "0") 
                   |> centered
                   |> size 20 
                   |> filled black 
                   |> move (-50, -5)] 

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