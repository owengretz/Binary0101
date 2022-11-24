-- https://cs1xd3.online/ShowModulePublish?modulePublishId=c22929e7-377a-4e1f-9fb7-988fa538da23
-- lesson 11 visual:
--   generate random colours and see their hex code & RGB value

-- define colours
highlightCol = rgb 169 229 187
buttonTextCol = rgb 100 100 100
buttonCol = rgb 230 230 230
shadowCol = rgb 13 145 121

myTitle = "Hexadecimal and Colours"

type alias Model = { time : Float
                   , red : Int
                   , blue : Int
                   , green : Int
                   , hovering : Int
                   }
init : Model
init = { time = 0
       , red = 65
       , green = 179
       , blue = 164
       , hovering = -2
       }
       
type Msg = Tick Float GetKeyState
          | NewNumber (List Int)
          | GenerateColour
          | Hovered Int
          | Leave
          | Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    Tick t _ ->
      ( { model | time = t }
      , Cmd.none )
    NewNumber xs -> ( { model | red = (getAtIndex xs 0 0)
                            , green = (getAtIndex xs 1 0) 
                            , blue = (getAtIndex xs 2 0) }, Cmd.none )
    GenerateColour -> (model , getRandChoices)
    
    Hovered num -> ( { model | hovering = num }, Cmd.none )
    Leave -> ( { model | hovering = -2 }, Cmd.none )
    
    -- A reset message just in case...
    Reset -> ( init, Cmd.none )
myShapes model = [ circle 10 |> filled (rgb (toFloat model.red) (toFloat model.green) (toFloat model.blue)) 
                     |> move (25,22)
                 , changeColourButton model |> move (0,0) |> notifyTap GenerateColour
                 , text "RGB:" |> centered |> size 5 |> filled black|> move (0,47)
                 , group [ text(Debug.toString model.red) |> centered |> filled red |> move (-25,0)
                         , text(Debug.toString model.blue) |> centered |> filled blue |> move (25,0)
                         , text(Debug.toString model.green) |> centered |> filled green |> move (0,0)
                         ] |> move (0,37)
                 , text "Hexadecimal:" |> centered |> size 5 |> filled black|> move (-20,27)
                 , text(hexadecimal model) |> centered |> filled (rgb (toFloat model.red) (toFloat model.green) (toFloat model.blue))
                     |> move (-20, 17)
                   
                 ]
                 
hexadecimal model ="#" ++ toHex model.red ++ toHex model.green ++ toHex model.blue

view : Model -> Collage Msg
view model = collage 192 128 (myShapes model)

-- Generate a list of three random choice of decimal numbers from 0 to 255
getRandChoices = Random.generate ( \ xs -> NewNumber xs) <| Random.list 3 (Random.int 0 255)

main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init
                        , Cmd.none)
        , update = update
        , view = \ model -> { title = myTitle, body = view model }
        , subscriptions = \_ -> Sub.none
        }

-- Helpful Functions

-- Converts a decimal number to a hexadecimal string
toHex : Int -> String
toHex num =
    String.fromList <|
        if num < 0 then
            '-' :: unsafePositiveToDigits [] (negate num)

        else
            unsafePositiveToDigits [] num
            
unsafePositiveToDigits : List Char -> Int -> List Char
unsafePositiveToDigits digits num =
    if num < 16 then
        unsafeToDigit num :: digits

    else
        unsafePositiveToDigits (unsafeToDigit (modBy 16 num) :: digits) (num // 16)

{-| ONLY EVER CALL THIS WITH INTEGERS BETWEEN 0 and 15!
-}
unsafeToDigit : Int -> Char
unsafeToDigit num =
    case num of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        15 ->
            'f'

        _ ->
            -- if this ever gets called with a number over 15, it will never
            -- terminate! If that happens, debug further by uncommenting this:
            --
            -- Debug.todo ("Tried to convert " ++ toString num ++ " to hexadecimal.")
            unsafeToDigit num
    
changeColourButton model = 
  group [ roundedRect 75 15 7.5 |> filled shadowCol |> move (2, -2)
        , roundedRect 75 15 7.5 
          |> filled (if model.hovering == -1 then highlightCol else buttonCol) 
        , text("Generate New Colour") |> centered |> size 6 |> filled buttonTextCol |> move (0, -2)
        ] 
        |> scale (if model.hovering == -1 then 1.1 else 1)
        |> notifyEnter (Hovered -1)
        |> notifyLeave Leave
        |> move (0, -2)
        
-- other helper functions
getAtIndex : List a -> Int -> a -> a
getAtIndex xs p default = fromMaybe (List.head (List.drop p xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default