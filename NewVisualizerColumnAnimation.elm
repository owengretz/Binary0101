-- https://cs1xd3.online/ShowModulePublish?modulePublishId=1731a824-39b3-45dd-b07f-a96fa3ac74f0
-- lesson 5 visual:
--   animate the column breakdown of a number
--   to show the similarity between decimal & binary

import Task

-- info needed by typing app
-- (needed to use typing app slot even though there is no typing
--  so that the Tick message doesn't have GetKeyState;
--  can't have GetKeyState because the main module also has
--  to be a typing app & therefore doesn't have the GetKeyState
--  to pass in update either)
appTitle = "Decimal & Binary Column Animation"
allowTyping model = False
isAnimating model = True
collageWidth = 192
collageHeight = 128

myShapes model =
  [ swapButton model |> move (60,40)
  ,
  group [
  -- binary
  if model.showingBase2 then
  (
  group [
      showTextLine model
          [ ("1",1)
          , ("0",2)
          , ("1",3)
          , ("0",4)
          , (" = ",1)
          , ("(1x8)",1)
          , (" + ",2)
          , ("(0x4)",2)
          , (" + ",3)
          , ("(1x2)",3)
          , (" + ",4)
          , ("(0x1)",4)
          ] binxPositions
      ,
      text ("(")
          |> centered
          |> size 10
          |> filled black
          |> move (-91, 0.5)
      ,
      text (")")
          |> centered
          |> size 10
          |> filled black
          |> move (-67, 0.5)
      ,
      text ("2")
          |> centered
          |> size 4
          |> filled black
          |> move (-65, -2)
      ,
      openPolygon (animPoly model 0 -87 10 50 -41 10)
          |> outlined (solid 1) (getCol 0)
      ,
      openPolygon (animPoly model (num2) -82 10 40 4 10)
          |> outlined (solid 1) (getCol 1)
      ,
      openPolygon (animPoly model (num2*2) -77 10 30 44 10)
          |> outlined (solid 1) (getCol 2)
      ,
      openPolygon (animPoly model (num2*3) -72 10 20 79 10)
          |> outlined (solid 1) (getCol 3)
      ,
      (List.indexedMap (\i idx ->
      let words = ["8s Column","4s Column","2s Column","1s Column"]
      in
          text (getAtIndex words idx "")
          |> size 5
          |> centered
          |> filled (getCol idx)
          |> move (-64,52 - 10 * (toFloat idx))
          |> makeTransparent (getTrans model (idx+1))
      )
      (List.range 0 4)
      ) |> group
      ]
  )
  -- decimal
  else
  group [
      showTextLine model
          [ ("3",1)
          , ("2",2)
          , ("3",3)
          , ("4",4)
          , (" = ",1)
          , ("(3x1000)",1)
          , (" + ",2)
          , ("(2x100)",2)
          , (" + ",3)
          , ("(3x10)",3)
          , (" + ",4)
          , ("(4x1)",4)
          ] xPositions
      ,
      openPolygon (animPoly model 0 -87 10 50 -41 10)
          |> outlined (solid 1) (getCol 0)
      ,
      openPolygon (animPoly model (num2) -82 10 40 4 10)
          |> outlined (solid 1) (getCol 1)
      ,
      openPolygon (animPoly model (num2*2) -77 10 30 44 10)
          |> outlined (solid 1) (getCol 2)
      ,
      openPolygon (animPoly model (num2*3) -72 10 20 79 10)
          |> outlined (solid 1) (getCol 3)
      ,
      (List.indexedMap (\i idx ->
      let words = ["1000s Column","100s Column","10s Column","1s Column"]
      in
          text (getAtIndex words idx "")
          |> size 5
          |> centered
          |> filled (getCol idx)
          |> move (-64,52 - 10 * (toFloat idx))
          |> makeTransparent (getTrans model (idx+1))
      )
      (List.range 0 4)
      ) |> group
      ]
      ] |> move (0,-10)
  ]

-- system to animate a polygon 
animPoly model timeOffset x1 y1 y2 x3 y4 = 
        let t = model.time - timeOffset
            totalTime = 1
            d1  = (y2-y1)
            d2  = (x3-x1)
            d3  = (y2-y4)
            dist = d1 + d2 + d3
            speed = dist / totalTime
            lt = totalTime / 3
        in
        if t > 0
        then if t < d1 / speed
             then [(x1, y1),(x1,y1+t*(dist/totalTime))] 
             else if t < (d1+d2) / speed
             then [(x1, y1),(x1,y2),(x1+(t-(d1 / speed))*(dist/totalTime),y2)] 
             else if t < dist / speed
             then [(x1, y1),(x1,y2),(x3,y2),(x3,y2-(t-((d1+d2)/speed))*(dist/totalTime))]
             else [(x1, y1),(x1,y2),(x3,y2),(x3,y4)]
        else []

-- position text properly
showTextLine model textList posList = 
    List.indexedMap
    (\i idx ->
    text (Tuple.first (getAtIndex textList idx ("",0)))
          |> centered
          |> size 10
          |> filled (getCol (-1 + Tuple.second (getAtIndex textList idx ("",0))))
          |> move (getAtIndex posList idx 0, 0)
          |> makeTransparent (getTrans model (Tuple.second (getAtIndex textList idx ("",0))))
          )
    (List.range 0 (List.length textList - 1)) |> group

-- get transparency & colour for animation
getTrans model idx = getAtIndex model.transparencies (idx-1) 0
getCol idx = case idx of
    0 -> rgb 100 143 255
    1 -> rgb 120 94 240
    2 -> rgb 220 38 127
    3 -> rgb 254 97 0
    otherwise -> black
num1 = 2
num2 = 2
changeTransparencies model = 
        let t = model.time
            p = 0.4
        in
        if t < num1
            then [t+p,p,p,p]
        else if t < num1 + num2
            then [1,t-num2+p,p,p]
        else if t < num1 + num2 * 2
            then [1,1,t-num2*2+p,p]
        else if t < num1 + num2 * 3
            then [1,1,1,t-num2*3+p]
        else [1,1,1,1]

-- text positions
binxPositions = 
    [ -87
    , -82
    , -77
    , -72
    , -58
    , -41
    , -17
    , 4
    , 25
    , 44
    , 63
    , 79
    ]
xPositions = 
    [ -87
    , -82
    , -77
    , -72
    , -65
    , -41
    , -17
    , 4
    , 25
    , 44
    , 63
    , 79
    ]

-- define button colours
highlightCol = rgb 169 229 187
buttonTextCol = rgb 100 100 100
buttonCol = rgb 230 230 230
shadowCol = rgb 13 145 121
swapButton model = 
  group [ roundedRect 55 15 7.5 |> filled shadowCol |> move (2, -2)
        , roundedRect 55 15 7.5 
          |> filled (if model.hovering then highlightCol else buttonCol) 
        , text("Swap") |> centered |> size 6 |> filled buttonTextCol |> move (0, -2)
        ] 
        |> scale (if model.hovering then 1.1 else 1)
        |> notifyEnter Hovered
        |> notifyLeave Leave
        |> notifyTap Swap
        |> move (0, -2)

type Msg = Tick Float
         | Swap
         | Hovered
         | Leave
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp

update msg model = 
    case msg of
        Tick t -> 
            ({ model
            | transparencies = changeTransparencies model
            , showingBase2 = if model.time > num1 + num2 * 3 + 1
                             then not model.showingBase2
                             else model.showingBase2
            , time = if model.time > num1 + num2 * 3 + 1
                     then 0
                     else model.time + (t - model.prevFrameTime)
            , prevFrameTime = t
            }, Cmd.none)
        Swap ->
            ({ model
            | showingBase2 = not model.showingBase2
            , time = 0
            }, Cmd.none)
        Hovered -> ( { model | hovering = True }, Cmd.none )
        Leave -> ( { model | hovering = False }, Cmd.none )
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

type alias Model = 
    { time : Float
    , transparencies : List Float
    , showingCol : Int
    , showingBase2 : Bool
    , prevFrameTime : Float
    , hovering : Bool
    , window : Window
    }

init : Model
init = 
    { time = 0
    , transparencies =[0,0,0,0]
    , showingCol = 1
    , showingBase2 = False
    , prevFrameTime = 0
    , hovering = False
    , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
    }

view model = collage 192 128 (myShapes model)

run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())

getAtIndex : List a -> Int -> a -> a
getAtIndex xs n default = fromMaybe (List.head (List.drop n xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default