-- https://cs1xd3.online/ShowModulePublish?modulePublishId=3e05dd2b-471f-4704-820f-5b70d47163ab
-- get arrow key inputs to use for lesson navigation

import Task

collageWidth = 192
collageHeight = 128
appTitle = "Arrow Key Input"
allowTyping model = model.state == NotTyping 
isAnimating model = False 

myShapes model =
  [
      text (model.debug) |> size 15 |> centered |> filled black
  ]

type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp
         | RightArrowPressed
         | LeftArrowPressed
         
type State = NotTyping 

type alias Model =  { state : State
                    , window : Window
                    , time : Float
                    , debug : String
                    }

init =  { state = NotTyping
        , time = 0
        , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
        , debug = "-"
        }

update msg model = 
  case msg of
    KeyUp _ -> (model,Cmd.none)
    KeyDown code -> (model,
                    case code of
                      "ArrowRight" -> run RightArrowPressed
                      "ArrowLeft" -> run LeftArrowPressed
                      otherwise -> Cmd.none)
    RightArrowPressed -> ({model|debug="RightArrow"},Cmd.none)
    LeftArrowPressed -> ({model|debug="LeftArrow"},Cmd.none)
    Tick t -> ( { model | time = t }, Cmd.none )
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

typeAndDelete soFar code =
    if String.length code == 1 then 
        code :: soFar 
    else if code == "Backspace" then
        List.drop 1 soFar
    else soFar
    
run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())