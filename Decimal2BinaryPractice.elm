-- https://cs1xd3.online/ShowModulePublish?modulePublishId=e4464393-5360-451a-b8ad-f5604e976e71
-- lesson 9 visual:
--   decimal to binary conversion practice question(s)

import Task

-- define colours
bgCol = rgb 65 179 164
highlightCol = rgb 169 229 187
buttonTextCol = rgb 100 100 100
buttonCol = rgb 230 230 230
shadowCol = rgb 13 145 121
greenCol = rgb 169 229 187
myWhite = rgb 220 220 220

myTitle = "Decimal to Binary Practice"

myShapes model =
  [ text (String.fromInt model.decimal) |> centered |> filled white |> move (0, 45)
  , equalSign
  , checkAnswerButton model |> notifyTap CheckAnswer
  , displayBinary model
  , showFeedback model
  , refreshQ model
  ]

-- converts a decimal number to binary for checking the answer
convertToBin n = 
    insertExtraZeroes (List.reverse (convertToBinHelper n))
convertToBinHelper d = case d of 
         0 -> []
         n -> if modBy 2 n == 0 then 0 :: convertToBinHelper (n // 2) 
              else 1 :: convertToBinHelper (n // 2)
-- insert leading zeroes so that we have an 8 bit binary number
insertExtraZeroes lst = insertExtraZeroesHelper (8 - List.length lst) lst
insertExtraZeroesHelper amount lst = List.repeat amount 0 ++ lst

changeAnswer ans changeIdx = 
  List.indexedMap (\i idx ->
    if idx == changeIdx
    then (if (getAtIndex ans idx 0) == 1 then 0 else 1)
    else getAtIndex ans idx 0)
  (List.range 0 7)

equalSign = List.map(\idx -> rect 1 10 |> filled white |> move (5 * toFloat idx - 2, 36))
              (List.range 0 1) |> group

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

-- each box changes between one and zero when the user clicks on it        
displayBinary model = 
  List.map (\idx -> 
    group [
      roundedRect 15 15 3 
        |> filled white
    , text(String.fromInt (getAtIndex model.ans idx 0)) 
        |> centered 
        |> size 8 
        |> filled buttonTextCol 
        |> move (0,-3)
    ]
    |> notifyTap (ChangeAns idx)
    |> notifyEnter (Hovered idx)
    |> notifyLeave Leave
    |> scale (if model.hovering == idx then 1.1 else 1)
    |> move (20 * toFloat idx - 70,20)
    )
  (List.range 0 7) |> group

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
   
type alias Model = { time : Float
                   , decimal : Int
                   , ans : List Int
                   , hovering : Int
                   , results : List Bool
                   }

init : Model
init = { time = 0
       , decimal = 0
       , ans = [0,0,0,0,0,0,0,0]
       , hovering = -3
       , results = []
       }

type Msg = Tick Float GetKeyState
          | NewNumber Int
          | Hovered Int
          | Leave
          | ChangeAns Int
          | CheckAnswer
          | ChangeQuestion

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
   case msg of

    Tick t _ ->
      ( { model | time = t }, Cmd.none )
    
    NewNumber decimal -> ( { model | decimal = decimal }, Cmd.none )
    
    Hovered num -> ( { model | hovering = num }, Cmd.none )
    
    Leave -> ( { model | hovering = -3 }, Cmd.none )
    
    ChangeAns idx -> ( { model | ans = changeAnswer model.ans idx  }, Cmd.none )
    
    CheckAnswer ->   let canCheck = getAtIndex model.results ((List.length model.results)-1) False /= True
                     in
                     ({ model 
                     | results = if canCheck
                                 then model.results ++ [if model.ans == convertToBin (model.decimal) then True else False]
                                 else model.results 
                     }, if List.length model.results >= 7 && model.ans /= convertToBin (model.decimal) && canCheck
                        then run ChangeQuestion else Cmd.none )
    
    ChangeQuestion -> ( init, getRandChoices )

view : Model -> Collage Msg
view model = collage 192 128 (myShapes model)

-- Generate a random choice of decimal number from 0 to 255
getRandChoices = Random.generate ( \ a -> NewNumber a ) <| Random.int 0 255

main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init  -- this is the initial state, like you are used to
                        , getRandChoices)-- this requests the first random number
        , update = update
        , view = \ model -> { title = myTitle, body = view model }
        , subscriptions = \_ -> Sub.none
        }

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