-- https://cs1xd3.online/ShowModulePublish?modulePublishId=b316242f-5683-41fd-af58-b8b1a3e8506e
-- lesson 10 visual:
--   displays binary addition, multiplication, and converting to octal
--   with a hovering effect

-- info needed by typing app
-- (needed to use typing app slot even though there is no typing
--  so that the Tick message doesn't have GetKeyState;
--  can't have GetKeyState because the main module also has
--  to be a typing app & therefore doesn't have the GetKeyState
--  to pass in update either)
appTitle = "More Binary Applications"
allowTyping model = True
isAnimating model = True
collageWidth = 192
collageHeight = 128

myShapes model =
  [ addition
      |> move (-25,43)
      |> move (0,3 * sin (1-0.8*model.time))
      |> rotate (0.1)
  , otherBase
      |> move (0,-3)
      |> move (0,3 * sin (model.time))
      |> rotate (-0.05)
  , multiplication
      |> move (60,50)
      |> move (0,3 * sin (4+1.1*model.time))
      |> rotate (-0.3)
  ]

otherBase =
  group [ text "(10010110)  = (226)  "
            |> size 8 |> centered |> filled black
        , text "2"
            |> size 4 |> centered |> filled black
            |> move (4,-2)
        , text "8"
            |> size 4 |> centered |> filled black
            |> move (31.5,-2)
        ] |> repaint white
  
addition = 
  group [ text "01001110" |> size 8 |> alignRight |> filled black
        , text "+ 00101011" |> size 8 |> alignRight |> filled black |> move (0.3,-10)
        , line (-40,-12) (5,-12) |> outlined (solid 0.5) black
        , text "= 01111001" |> size 8 |> alignRight |> filled black |> (move (0,-20))
        ] |> repaint white
        
multiplication = 
  group [ text "00010000 " |> size 8 |> alignRight |> filled black
        , text "x 00001101" |> size 8 |> alignRight |> filled black |> move (-2,-10)
        , line (-43,-12) (2,-12) |> outlined (solid 0.5) black
        , text "= 11010000" |> size 8 |> alignRight |> filled black |> (move (-2.5,-20))
        ] |> repaint white

type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp

type alias Model = { time : Float, window : Window }

update msg model = case msg of
                     Tick t -> ({ model | time = t }, Cmd.none)
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