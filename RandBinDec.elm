-- https://cs1xd3.online/ShowModulePublish?modulePublishId=3986ca4c-503b-482f-bb34-73761ee1275b
-- lesson 1 visual: 
--   random number generation to show examples
--   of the difference between decimal & binary

-- define colours
highlightCol = rgb 169 229 187
buttonTextCol = rgb 100 100 100
buttonCol = rgb 230 230 230
shadowCol = rgb 13 145 121

myShapes model = 
  [ button model 1 GenNum -- button
  , text model.decNum -- decimal number
      |> centered
      |> size 10
      |> filled black
      |> move (-40,15)
  , text model.binNum -- binary number
      |> centered
      |> size 10
      |> filled black
      |> move (40,15)
  , text "Decimal:"
      |> centered
      |> size 6
      |> filled black
      |> move (-40,29)
  , text "Binary:"
      |> centered
      |> size 6
      |> filled black
      |> move (40,29)
  ]

button model num msg = group [
  roundedRect 70 15 7.5|> filled shadowCol |> move (2,-2)
  ,
  roundedRect 70 15 7.5|> filled (if model.buttonHoverNum == num then highlightCol else buttonCol)
  ,
  text ("Generate Number") |> size 8 |> centered |> filled buttonTextCol |> move (0,-3)
  ] |> notifyTap msg
    |> notifyEnter (StartHover num)
    |> notifyLeave StopHover
    |> scale (if model.buttonHoverNum == num then 1.1 else 1)

type Msg
    = Tick Float GetKeyState
    | GenNum
    | NewNum Int
    | StartHover Int
    | StopHover

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ -> 
            ( { model | time = t }, Cmd.none )
        GenNum ->
            ( model, Random.generate NewNum (Random.int 0 255) )
        NewNum number ->
            ( {model | decNum = String.fromInt number, binNum = convertToBin number }, Cmd.none )
        StartHover buttonNum ->
            ( {model | buttonHoverNum = buttonNum }, Cmd.none )
        StopHover ->
            ( {model | buttonHoverNum = 0 }, Cmd.none )

-- converts a decimal number to binary
convertToBin n = 
    lst2str (insertExtraZeroes (List.reverse (convertToBinHelper n)))
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

type alias Model =
    { time : Float
    , decNum : String
    , binNum : String
    , buttonHoverNum : Int
    }

init : Model
init =
    { time = 0
    , decNum = "0"
    , binNum = "00000000"
    , buttonHoverNum = 0
    }

view model = collage 192 128 (myShapes model) 

main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> (init, Cmd.none)
        , view = \model -> { title = "Title", body = view model }
        , update = update
        , subscriptions = \_ -> Sub.none
        }