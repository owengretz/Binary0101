-- https://cs1xd3.online/ShowModulePublish?modulePublishId=50670a31-2b65-4aa6-97f8-41899297fece
-- lesson 8 visual:
--   binary to decimal conversion practice question(s)

import Task

-- define colours
bgCol = rgb 65 179 164
highlightCol = rgb 169 229 187
buttonTextCol = rgb 100 100 100
buttonCol = rgb 230 230 230
shadowCol = rgb 13 145 121
greenCol = rgb 169 229 187
myWhite = rgb 220 220 220

-- info needed by typing app
appTitle = "Binary To Decimal Practice"
allowTyping model = True
isAnimating model = False
collageWidth = 192
collageHeight = 128

myShapes model =
  [ text (lst2str (convertToBin (getDec model))) |> centered |> filled white |> move (0, 45)
  , equalSign
  , textBox 40 15 15 model.chars |> move (0,18)
  , refreshQ model
  , checkAnswerButton model |> notifyTap CheckAnswer
  , showFeedback model
  ]

textBox width height textSize chars =
  [ roundedRect width height 5 |> filled white
  , text (String.join "" <| List.reverse chars ) 
      |> centered 
      |> size textSize
      |> filled black 
      |> move (0,-textSize/3)
      |> clip (rect width height |> ghost)
  , roundedRect width height 5 |> outlined (solid 1) black
  ] |> group

equalSign = List.map(\idx -> rect 1 10 |> filled white |> move (5 * toFloat idx - 2, 36))
              (List.range 0 1) |> group

refreshQ model = group [ circle 6 |> filled white
                , circle 4.5 |> filled bgCol
                , square 3.5 |> filled bgCol |> move(5, 1)
                , rect 1.5 4 |> filled white |> rotate (pi/2) |> move (3, 3)
                , rect 1.5 4 |> filled white |> rotate (pi) |> move (4.7,4.4)
                    ]
                |> scale (if model.hovering == -2 then 1.1 else 1)
                |> move (60,45)
                |> notifyTap ChangeQuestion
                |> notifyEnter (Hovered -2)
                |> notifyLeave Leave

xMark = group [ circle 8 |> filled red |> makeTransparent 0.9
                   , circle 8 |> outlined (solid 0.9) darkRed
                   , group [ roundedRect 3 12 1 |> filled white |> rotate (degrees 90)
                           , roundedRect 3 12 1 |> outlined (solid 0.5) myWhite |> rotate (degrees 90)
                           , roundedRect 3 12 1 |> filled white |> rotate (degrees 180)
                           , roundedRect 3 12 1 |> outlined (solid 0.5) myWhite |> rotate (degrees 180)
                           ,  roundedRect 2 10 1 |> filled white |> rotate (degrees 90)]
                         |> rotate (degrees 45)
                         ]

checkMark = group [ circle 13 |> filled (rgb 13 145 121) 
              , circle 12 |> filled white 
              , [ rect 5 10 |> filled greenCol |> rotate (degrees 56) |> move (5, -3)
                , rect 5 15 |> filled greenCol |> rotate (degrees -30) |> move (10, 0)
                ] |> group 
                  |> move (-8, -1)
              ] |> scale 0.7

-- display X marks and check marks
showFeedback model =
  List.indexedMap (\i idx ->
    (if getAtIndex model.results idx True == True then checkMark else xMark) 
         |> move (42+6 * (toFloat idx),-2)
    )
  (List.range 0 ((List.length model.results)-1))
  |> group

checkAnswerButton model = 
  group [ roundedRect 55 15 7.5 |> filled shadowCol |> move (2, -2)
        , roundedRect 55 15 7.5 
          |> filled (if model.hovering == -1 then highlightCol else buttonCol) 
        , text("Check Answer") |> centered |> size 6 |> filled buttonTextCol |> move (0, -2)
        ] 
        |> scale (if model.hovering == -1 then 1.1 else 1)
        |> notifyEnter (Hovered -1)
        |> notifyLeave Leave
        |> move (0, -2)

type Msg = Tick Float
         | Hovered Int
         | Leave
         | CheckAnswer
         | ChangeQuestion
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp
         -- need to import another module because
         -- the typing app slot doesn't have Random
         | RandNumMsg Gretzino.RandBinNum.Msg

type alias Model =  {
                      window : Window
                    , time : Float
                    , chars : List String
                    , results : List Bool
                    , hovering : Int
                    , randNumModel : Gretzino.RandBinNum.Model
                    }

init =  {
          time = 0
        , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
        , chars = []
        , results = []
        , hovering = -3
        , randNumModel = Gretzino.RandBinNum.init
        }

getAns : Model -> Int
getAns model = fromMaybe (String.toInt (String.reverse (String.join "" model.chars))) 0
getDec model = model.randNumModel.num

update msg model = 
  case msg of
    Hovered num -> ( { model | hovering = num }, Cmd.none )
    Leave -> ( { model | hovering = -3 }, Cmd.none )
    CheckAnswer ->   let canCheck = getAtIndex model.results ((List.length model.results)-1) False /= True
                     in
                     ({ model 
                     | results = if canCheck
                                 then model.results ++ [if getAns model == getDec model then True else False]
                                 else model.results 
                     }, if List.length model.results >= 7 && getAns model /= getDec model && canCheck
                        then run ChangeQuestion else Cmd.none )
    ChangeQuestion ->
      let 
        (newModel, newCmd) = Gretzino.RandBinNum.update Gretzino.RandBinNum.GenerateRandomNumber model.randNumModel
      in
        ({ init | randNumModel = newModel }, Cmd.map RandNumMsg newCmd)
    RandNumMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.RandBinNum.update imsg model.randNumModel
          in
            ({ model | randNumModel = newModel }, Cmd.map RandNumMsg newCmd)
    KeyUp _ -> (model,Cmd.none)
    KeyDown code -> ({ model | chars =  
                          if (String.all Char.isDigit code && List.length model.chars < 3)
                              || code == "Backspace"
                          then typeAndDelete model.chars code
                          else model.chars }
                    ,if code == "Enter"
                     then run CheckAnswer
                     else Cmd.none)
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

convertToBin n = 
    insertExtraZeroes (List.reverse (convertToBinHelper n))
convertToBinHelper d = case d of 
         0 -> []
         n -> if modBy 2 n == 0 then 0 :: convertToBinHelper (n // 2) 
              else 1 :: convertToBinHelper (n // 2)
-- insert leading zeroes so that we have an 8 bit binary number
insertExtraZeroes lst = insertExtraZeroesHelper (8 - List.length lst) lst
insertExtraZeroesHelper amount lst = List.repeat amount 0 ++ lst
lst2str lst = 
  case lst of
    [] -> ""
    x::xs -> String.fromInt x ++ lst2str xs

-- other helper functions
run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())

getAtIndex : List a -> Int -> a -> a
getAtIndex xs p default = fromMaybe (List.head (List.drop p xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default