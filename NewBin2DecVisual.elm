-- https://cs1xd3.online/ShowModulePublish?modulePublishId=9b7e9e7c-55d0-409d-9a87-d7a8cbac16f2
-- lesson 6 visual:
--   demonstrate how to convert a binary number to decimal

-- define colours
textCol = white
boxCol = rgb 5 80 70

-- info needed by typing app
-- (needed to use typing app slot even though there is no typing
--  so that the Tick message doesn't have GetKeyState;
--  can't have GetKeyState because the main module also has
--  to be a typing app & therefore doesn't have the GetKeyState
--  to pass in update either)
appTitle = "Binary To Decimal Visual"
allowTyping model = True
isAnimating model = True
collageWidth = 192
collageHeight = 128

myShapes model =
  [ text "Binary Number: 1101" |> size 5 |> centered |> filled textCol |> move (0,50)
  , textInBox ["1","1","0","1"] 30 12 15 15 38 |> group
  , textInBox ["1 x (2^3)","1 x (2^2)","0 x (2^1)","1 x (2^0)"] 45 7 35 12 15 |> group
  , plusSigns 45 15 7 |> group
  , text "13" |> size 10 |> centered |> filled textCol |> move (0,-10)
  , text "Decimal Equivalent" |> size 5 |> centered |> filled textCol |> move (0,-17)
  , firstLines model.time 30 38 15 45 15 12 |> group
  , secondLines 45 15 12 3 |> group
  , line (-68,3) (68,3) |> outlined (solid 1) boxCol
  , line (0,3) (smoothPt (0,3) (0,-2) (model.time / animSpd)) |> outlined (solid 1) boxCol
  ]

-- format text inside boxes
textInBox txtList spacing textSize boxWidth boxHeight yPos =
  List.indexedMap (\i idx ->
    group [ text (getAtIndex txtList idx "")
              |> size textSize
              |> centered
              |> filled textCol
              |> move (0,-textSize/3)
          , rect boxWidth boxHeight
              |> outlined (solid 1) boxCol
    ] |> move (-spacing*1.5 + spacing * (toFloat idx), yPos)
  )
  (List.range 0 3)

plusSigns spacing yPos textSize = 
  List.indexedMap (\i idx ->
    text "+"
      |> size textSize
      |> centered
      |> filled textCol
      |> move (-spacing + spacing * (toFloat idx), yPos - textSize/3)
  )
  (List.range 0 2)

firstLines time topSpacing topY topHeight bottomSpacing bottomY bottomHeight = 
  List.indexedMap (\i idx ->
    let firstPt = (-topSpacing*1.5 + topSpacing * (toFloat idx),topY-topHeight/2)
        secondPt = (-bottomSpacing*1.5 + bottomSpacing * (toFloat idx),bottomY+bottomHeight/2-0.1)
    in
    line firstPt
         (smoothPt firstPt secondPt (time / animSpd))
      |> outlined (solid 1) boxCol
  )
  (List.range 0 3)
smoothPt (p1x,p1y) (p2x,p2y) t = (p1x + (p2x-p1x)*t,p1y + (p2y-p1y)*t)
animSpd = 1.5

secondLines spacing topY topHeight bottomY = 
  List.indexedMap (\i idx ->
    let firstPt = (-spacing*1.5 + spacing * (toFloat idx),topY-topHeight/2)
        secondPt = (-spacing*1.5 + spacing * (toFloat idx),bottomY)
    in
    line firstPt secondPt
      |> outlined (solid 1) boxCol
  )
  (List.range 0 3)

type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp

type alias Model = { time : Float, timeOffset : Float, window : Window }

update msg model = case msg of
                     Tick t   -> ({ model 
                                 | time = t - model.timeOffset
                                 , timeOffset = if t - model.timeOffset > animSpd 
                                                then t 
                                                else model.timeOffset
                                 }, Cmd.none)
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

init = { time = 0, timeOffset = 0, window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }}

view model = collage 192 128 (myShapes model)

-- other helper functions
getAtIndex : List a -> Int -> a -> a
getAtIndex xs p default = fromMaybe (List.head (List.drop p xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default