-- https://cs1xd3.online/ShowModulePublish?modulePublishId=0a1a2093-dd2e-41de-9657-5782407fa85a
-- end visual:
--   congratulations & letter animation
--   shows "Certificate of Completion"

-- info needed by typing app
-- (needed to use typing app slot even though there is no typing
--  so that the Tick message doesn't have GetKeyState;
--  can't have GetKeyState because the main module also has
--  to be a typing app & therefore doesn't have the GetKeyState
--  to pass in update either)
appTitle = "Credits"
allowTyping model = False
isAnimating model = True
collageWidth = 192
collageHeight = 128

-- define colours
highlightCol = rgb 169 229 187
buttonTextCol = rgb 100 100 100
buttonCol = rgb 230 230 230
shadowCol = rgb 13 145 121
greenCol = rgb 169 229 187

myShapes model = 
  [ 
  (case model.state of 
     LetterClosed -> 
       [ text("Congratulations!") |> centered |> size 20 |> filled white
           |> scale (if model.time < 1 then model.time else 1)
           |> move (0,-20/3)
           |> makeTransparent (if model.time > 2 then 0 else 1)
       , letter model
           |> move (0, (if model.time > 2.5 && model.time < 5 then (repeatDuration -10 5 50 model.time) else 0))
           |> makeTransparent (if model.time > 2.5 then 1 else 0)
       ]
     LetterOpen -> 
       [ roundedRect 150 100 2 |> filled greenCol
       , roundedRect 140 90 2 |> filled white
       , rect 130 0.5 |> filled hotPink |> move (0, 29)
       , text("Certificate of Completion") |> centered |> filled black |> move (0, 30)
       , text("Thank You For Completing") |> centered |> size 8 |> filled black |> move (0, 10)
       , text("Binary 0101!") |> centered |> size 8 |> filled black |> move (0, 2)
       , group [ ngon 12 10 |> filled yellow 
               , ngon 12 9 |> outlined (solid 0.5) white
               , text("Binary 0101") |> centered |> size 3 |> filled white |> move (0, -1)
               ] |> scale 1.5
                 |> move (0,-20)
       , text("01000010 01101001 01100111 00100000 01010111 01100101 01100010 01001001 01000100 01000101 00100000 01001100") |> centered |> size 2.7 |> filled black |> move (0, -44)
       ]
     ) |> group 
       |> scale 0.7
       |> move (0,20)
     ]

letter model = group [
   roundedRect 100 60 2 |> filled shadowCol |> move (3, -3)
 , roundedRect 100 60 2 |> filled buttonCol
 , roundedRect 100 60 2 |> filled (if model.hovering == 1 then highlightCol else buttonTextCol)
     |> clip (group [ roundedRect 60 4 2 |> filled (if model.hovering == 1 then highlightCol else buttonTextCol) |> move (28,0) |> rotate (5*pi/6)
                    , roundedRect 60 4 2 |> filled (if model.hovering == 1 then highlightCol else buttonTextCol) |> move (-28, 0) |> rotate (7*pi/6)
         ])
 ] |> scale (if model.hovering == 1 then 1.1 else 1)
   |> makeTransparent (1-(3-model.time)*2)
   |> notifyEnter (if model.time > 2.5 then (Hovered 1) else Empty)
   |> notifyLeave (if model.time > 2.5 then (Leave) else Empty)
   |> notifyTap (if model.time > 2.5 then Open else Empty)

type State = LetterClosed | LetterOpen

type Msg = Tick Float
          | Hovered Int
          | Leave
          | Open
          | Empty
          | WindowResize (Maybe ( Float, Float ))
          | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
          | KeyDown String
          | KeyUp String
          | NoOp

type alias Model = { time : Float
                   , state : State
                   , hovering : Int
                   , window : Window
                   }

update msg model = case msg of
                     Tick t -> ({ model | time = t },Cmd.none)
                     Hovered num -> case model.state of
                             LetterClosed -> ({ model | hovering = num },Cmd.none)
                             otherwise -> (model,Cmd.none)
                     Leave -> case model.state of 
                             LetterClosed -> ({ model | hovering = 0 },Cmd.none)
                             otherwise -> (model,Cmd.none)
                     Open -> case model.state of 
                             LetterClosed -> ({ model | state = LetterOpen },Cmd.none)
                             otherwise -> (model,Cmd.none)
                     Empty -> (model,Cmd.none)
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
                             
init = { time = 0
       , hovering = 0
       , state = LetterClosed
       , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
  }

view model = collage 192 128 (myShapes model)
