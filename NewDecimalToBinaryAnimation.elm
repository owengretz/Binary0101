-- https://cs1xd3.online/ShowModulePublish?modulePublishId=0a06f461-ed9f-48fe-8327-a0356abcae9e
-- lesson 7 visual:
--   demonstrate how to convert decimal to binary

-- info needed by typing app
-- (needed to use typing app slot even though there is no typing
--  so that the Tick message doesn't have GetKeyState;
--  can't have GetKeyState because the main module also has
--  to be a typing app & therefore doesn't have the GetKeyState
--  to pass in update either)
appTitle = "Decimal To Binary Animation"
allowTyping model = False
isAnimating model = True
collageWidth = 192
collageHeight = 128

myShapes model =
  [ displayDivisionLine model
  , display2 model
  , displayNum model
  , displayRemainders model
  , displayRemainder model
  , remainderLine model 
  ] 

myNums = ["157", "78", "39", "19", "9", "4", "2", "1"]
-- display numbers aligned vertically
displayNum model = List.map (\idx -> text (index myNums idx "") |> centered |> filled (getCol idx) |> move (-60, -15.2 * toFloat idx + 49)
                                |> makeTransparent ( -1 + model.time - toFloat idx))
                    (List.range 0 7) |> group

displayDivisionLine model = List.map (\idx -> openPolygon (animPoly model (toFloat idx * 1.5) 0 0 -15 -30 -15)
                                              |> outlined (solid 1) (getCol idx)
                                              |> move (-75, -15 * toFloat idx + 60))
                            (List.range 0 7) |> group
-- display 2s on the left to show division by 2 each time
display2 model = List.map (\idx -> text ("2") |> centered |> filled (getCol 8) |> move(-80, -15 * toFloat idx + 49)
                              |> makeTransparent (-1.5 + model.time -  toFloat idx))
            (List.range 0 7) |> group

remainders = ["1", "0", "1", "1", "1", "0", "0", "1"]
-- remainder text
displayRemainder model = List.map (\idx ->  text ("remainder") |> size 5 |> centered |> filled (getCol 8) |> move (-5 * toFloat idx, -15 * toFloat idx + 55)
                                      |> makeTransparent (-2.6 + model.time - 1.05* toFloat idx - 1))
                    (List.range 0 7) |> group
-- actual remainder values
displayRemainders model = List.map (\idx -> text (index remainders idx "") |> centered |> filled (getCol idx) |> move (-15 * toFloat idx + 85, -60)
                              |> makeTransparent (-3.4 + model.time - toFloat idx))
                             (List.range 0 7) |>group 
-- lines connecting from vertical numbers to corresponding remainders
remainderLine model = group [openPolygon (animPoly2 model 2 -130 102 0 102)
                               |> outlined (solid 1) (getCol 8)
                               |> move (85,-50) 
                           , openPolygon (animPoly2 model 3.5 -115 88 0 88)
                               |> outlined (solid 1) (getCol 8)
                               |> move (70,-50)
                           , openPolygon (animPoly2 model 5 -100 73 0 73)
                               |> outlined (solid 1) (getCol 8)
                               |> move (55,-50)
                            , openPolygon (animPoly2 model 6.5 -85 58 0 58)
                               |> outlined (solid 1) (getCol 8)
                               |> move (40,-50)
                            , openPolygon (animPoly2 model 8 -70 43 0 43)
                               |> outlined (solid 1) (getCol 8)
                               |> move (25,-50)
                            , openPolygon (animPoly2 model 9.5 -55 28 0 28)
                               |> outlined (solid 1) (getCol 8)
                               |> move (10,-50)
                            , openPolygon (animPoly2 model 11 -40 13 0 13)
                               |> outlined (solid 1) (getCol 8)
                               |> move (-5,-50)
                            , openPolygon (animPoly2 model 12.5 -20 0 0 0)
                               |> outlined (solid 1) (getCol 8)
                               |> move (-25,-54.5)
                               ]

-- systems to animate polygons
animPoly model timeOffset x1 y1 y2 x3 y4 = 
        let t = model.time - timeOffset
            totalTime = 1
            d1  = (y2-y1)
            d2  = (x3-x1)
            d3  = (y2-y4)
            dist = d1 + d2 + d3
            speed = dist / totalTime
        in
        if t > 0
        then if t < d1 / speed
             then [(x1, y1),(x1,y1+t*(dist/totalTime))] 
             else if t < dist / speed
             then [(x1, y1),(x1,y2),(x1-(t-(d1 / speed))*(dist/totalTime),y2)]
             else [(x1, y1),(x1,y2),(-x3,y2),(-x3,y4)]
        else []
animPoly2 model timeOffset x1 y1 x2 y2 = 
      let t = model.time - timeOffset
          totalTime = 1
          dx = (x2-x1)
          dy = y2
          dist = dx + dy
          speed = dx + dy / totalTime
      in if t > 0
      then if t < dx / speed
             then [(x1, y1),(x1+t*speed,y1)] 
             else if t < dist / speed
             then [(x1, y1),(x2,y2),(x2, y2-t*speed + (-x1))]
             else [(x1,y1),(x2,y2),(x2,0)]
      else []
        
getCol idx = case idx of
    0 -> rgb 100 143 255
    1 -> rgb 120 94 240
    2 -> rgb 220 38 127
    3 -> rgb 254 97 0
    4 -> rgb 100 143 255
    5 -> rgb 120 94 240
    6 -> rgb 220 38 127
    7 -> rgb 254 97 0
    8 -> rgb 80 80 80
    otherwise -> black
    
type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp

type alias Model = { time : Float, window : Window }

update msg model = case msg of
                     Tick t -> ({model | time = t }, Cmd.none)
                     KeyUp _ -> (model,Cmd.none)
                     KeyDown _ -> (model,Cmd.none)
                     WindowResize mWH ->
                        case mWH of
                          Just ( w, h ) ->
                            ( { model | window = didResize model.window w h
                                }
                            , Cmd.none
                            )
                          Nothing ->
                            ( model
                            , getViewportSize
                            )
                     ReturnPosition message ( x, y ) ->
                          let
                              ( newModel, userCmds ) =
                                  update
                                      (message (convertCoords model.window ( x, y ) ))
                                      model
                          in
                          ( newModel, userCmds )
                     NoOp -> ( model, Cmd.none )

init = { time = 0, window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 } }

view model = collage 192 128 (myShapes model)

-- other helper functions
index : List a -> Int -> a -> a
index xs n default = fromMaybe (List.head (List.drop n xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default