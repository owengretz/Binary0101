-- https://cs1xd3.online/ShowModulePublish?modulePublishId=2416e5c2-1534-4e38-88a8-55a4012a0e69
-- lesson 12 visual:
--   convert text to decimal & binary ASCII values

-- info needed by typing app
appTitle = "ASCII Converter"
allowTyping model = True
isAnimating model = True
collageWidth = 192
collageHeight = 128

-- convert a list of strings to a list of characters
newList lst = List.reverse (String.toList(String.concat lst))

charsToDeci lst = 
             case lst of 
                 [] -> []
                 (c::cs) -> Char.toCode c :: charsToDeci cs

charsToBin lst = 
            case lst of
              [] -> []
              (c::cs) -> convertToBin (Char.toCode c) :: charsToBin cs
              
displayDeci model lst = 
  case lst of 
    [] -> text ("") |> ghost
    (x::xs) -> 
      if List.length lst > 6
      then
        group [ displayDeci model (List.take 6 lst)
              , text "[...]" |> centered |> size 4 |> filled black |> move (34,-4/3)
              ]
      else 
        List.map(\idx ->
          text (String.fromInt(index lst idx 1))
            |> centered
            |> size 4
            |> filled black 
            |> move (deciListDisplay idx (List.length lst), -4/3))
        (List.range 0 (-1 + List.length lst)) |> group

displayBinLists model = 
  let len = if List.length model.chars > 3 then 3 else List.length model.chars
  in
  group [
  List.map (\idx -> displayBin model (index (charsToBin(newList (List.drop (List.length model.chars-len) model.chars))) idx [])
    |> move (binListsDisplay idx len, 0))
  (List.range 0 len) |> group
  ,
  if List.length model.chars > 3
  then text "[...]" |> centered |> size 4 |> filled black |> move (32,-4/3)
  else group []
  ]
                                      
displayBin model binList = 
  case binList of 
    -- so that it shows nothing when no text is entered
    [] -> text ("") |> ghost
    (x::xs) -> List.map (\idx -> 
                 text (String.fromInt (index binList idx 0))
                   |> centered
                   |> size 4
                   |> filled black
                   |> move (binDisplayFormula idx, -4/3)
        )
        (List.range 0 7) |> group
                  
-- converts a decimal number to binary
convertToBin n = 
    insertExtraZeroes (List.reverse (convertToBinHelper n ))
    
convertToBinHelper d = case d of 
         0 -> []
         n -> if modBy 2 n == 0 then 0 :: convertToBinHelper (n // 2) 
              else 1 :: convertToBinHelper (n // 2)
              
binDisplayFormula idx = -7 + 2 * toFloat idx

binListsDisplay idx elements = -9 * (toFloat elements - 1) + 18 * toFloat idx

deciListDisplay idx elements = -5 * (toFloat elements - 1) + 10 * toFloat idx
              
-- insert leading zeroes so that we have an 8 bit binary number
insertExtraZeroes lst = insertExtraZeroesHelper (8 - List.length lst) lst
insertExtraZeroesHelper amount lst = List.repeat amount 0 ++ lst

myShapes model =
  [ typeBox 80 20 5 model.chars |> move (-45, 12.5)
  , displayBinBox model 80 20 |> move (45, -5)
  , displayDeciBox model 80 20 |> move (45, 30)
  ]

displayDeciBox model width height = 
 group [ text "Decimal ASCII:" |> centered |> size 6 |> filled black |> move (0,13)
       , roundedRect width height 5 |> filled white 
       , displayDeci model (charsToDeci (newList model.chars))
           |> clip (rect width height |> ghost)
  ]

displayBinBox model width height =
  group [ text "Binary ASCII:" |> centered |> size 6 |> filled black |> move (0,13)
        , roundedRect width height 5 |> filled white 
        , displayBinLists model
            |> clip (rect width height |> ghost)
  ]

typeBox width height textSize chars =
  [ text "Type Here:" |> centered |> size 6 |> filled black |> move (0,13)
  , roundedRect width height 5 |> filled white
  , text (String.join "" <| List.reverse chars ) 
      |> centered 
      |> size textSize
      |> filled black 
      |> move (0,-textSize/3)
      |> clip (rect width height |> ghost)
  , roundedRect width height 5 |> outlined (solid 1) black
  ] |> group

type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp
  
type State = TypingBox1 | NotTyping 

type alias Model =  { state : State
                    , chars : List String
                    , window : Window
                    , time : Float
                    }
                    
init : Model                    
init =  { state = NotTyping 
        , chars = []
        , time = 0
        , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 } 
        }

update msg model = 
  case msg of
    KeyUp _ -> (model,Cmd.none)
    KeyDown code -> ({ model | chars = typeAndDelete model.chars code }
                    ,Cmd.none)
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
    
-- other helper functions
index : List a -> Int -> a -> a
index xs n default = fromMaybe (List.head (List.drop n xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default