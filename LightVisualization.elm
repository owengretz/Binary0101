-- https://cs1xd3.online/ShowModulePublish?modulePublishId=0f3d1a74-7615-4450-b24c-625557d90c02
-- lesson 4 visual:
--   8 lights to represent an 8 bit binary number
--   (there's also a button to explicitly see binary bits)
--   users can click on the lights/bits 
--     to see how it affects the decimal number
--   or click on the arrows to change the decimal number
--     and see how it affects the lights/bits

-- define colours
lightOffColour = rgb 215 204 182
textColour = black
highlightCol = rgb 169 229 187
shadowCol = rgb 13 145 121
buttonCol = rgb 230 230 230
buttonTextCol = rgb 100 100 100

myShapes model =
  [ group [ arrowButton model 1 1 90 10 -- up arrow
          , arrowButton model 2 -1 270 -10 -- down arrow
          , displayDecimalNum (model.deciNum)
          , swapButton model
          ]
  , displayBinary (convertToBin model.deciNum) model.showLights
      |> move (0,20)
  ]


swapButton model = 
    group [
      roundedRect 70 15 7.5|> filled shadowCol |> move (2,-2)
      ,
      roundedRect 70 15 7.5|> filled (if model.hovered == 0 then highlightCol else buttonCol)
      ,
      text "Swap Lights/Binary" |> size 6 |> centered |> filled buttonTextCol |> move (0,-2)
    ]
    |> scale (if model.hovered == 0 then 1.1 else 1)
    |> move (50,0)
    |> notifyEnter (Hovered 0)
    |> notifyLeave StopHover
    |> notifyTap SwapLights

displayDecimalNum n = text (String.fromInt n) 
                          |> centered
                          |> filled textColour
                          |> move (0, -4)
arrowButton model buttonNum dir rotation yPos =
    group [ triangle 7
              |> filled shadowCol
              |> rotate (degrees rotation)
              |> move (1,-1)
          , triangle 7
              |> filled buttonCol
              |> rotate (degrees rotation)
          ]
      |> scale (if (model.hovered == buttonNum) then 1.2 else 1)
      |> move (0, yPos)
      |> notifyEnter (Hovered buttonNum) |> notifyLeave StopHover
      |> notifyTap (ChangeDeci dir)

-- when changing the number, if it is above or below the limit wrap around
checkWrap n =      if n < 0 then 255
              else if n > 255 then 0
              else n

displayBinary binList asLights = 
    (List.map (\idx -> 
        [
        -- drawing binary 0/1 text
        (if not asLights
         then text (String.fromInt (index binList idx 0))
                  |> centered
                  |> size 18
                  |> filled textColour
                  |> move (binDisplayFormula idx, 9)
         else [] |> group)
        ,
        ((if asLights
        -- draw light bulbs
        then light ((index binList idx 0) == 1)
        -- draw invisible rects on top of binary numbers
        else square 18 |> filled white |> makeTransparent 0)
            |> move (binDisplayFormula idx, 15)
            |> notifyTap (ChangeBin (lightIdxToDec idx ((index binList idx 0) == 1))))
        ,
        text (String.fromInt (2 ^ (7-idx)))
                  |> centered
                  |> size 5
                  |> filled textColour
                  |> move (binDisplayFormula idx, 9)
                  |> move (0, if asLights then -10 else -10)
        ] |> group)
    (List.range 0 7)) |> group
-- gets the value of a binary digit in decimal
lightIdxToDec idx on = 2 ^ (7 - idx) * (if on then -1 else 1)
-- re-use this formula so put it in it's own function
binDisplayFormula idx = -73.5 + 21 * toFloat idx

-- converts a decimal number to binary
convertToBin n = 
    insertExtraZeroes (List.reverse (convertToBinHelper (checkWrap n)))
convertToBinHelper d = case d of 
         0 -> []
         n -> if modBy 2 n == 0 then 0 :: convertToBinHelper (n // 2) 
              else 1 :: convertToBinHelper (n // 2)
-- insert leading zeroes so that we have an 8 bit binary number
insertExtraZeroes lst = insertExtraZeroesHelper (8 - List.length lst) lst
insertExtraZeroesHelper amount lst = List.repeat amount 0 ++ lst

-- draw light to screen
light on = group [ 
                oval 15 17 |> filled lightOffColour
              , oval 15 17 |> if on
                              then filled yellow
                              else outlined (solid 1) yellow 
              , roundedRect 9 2 1 |> filled charcoal |> move (0, 8)
              , roundedRect 8 2 1 |> filled charcoal |> move (0, 10.5)
              , roundedRect 7 2 1 |> filled charcoal |> move (0, 13)
              , rect 1 50 |> filled charcoal |> move (0, 39)]

type Msg = Tick Float GetKeyState
           | ChangeDeci Int
           | ChangeBin Int
           | Hovered Int 
           | StopHover 
           | SwapLights

type alias Model = { time : Float 
                   , deciNum : Int
                   , hovered : Int
                   , showLights : Bool
                   }

update : Msg -> Model -> Model
update msg model = case msg of
        Tick t _ -> { model | time = t }
        ChangeDeci dir -> {model | deciNum = checkWrap (model.deciNum + dir)}
        ChangeBin amount -> {model | deciNum = checkWrap (model.deciNum + amount)}
        Hovered num -> {model | hovered = num}
        StopHover -> {model | hovered = -1}
        SwapLights -> {model | showLights = not model.showLights}

init : Model
init = { time = 0 
       , deciNum = 0
       , hovered = -1
       , showLights = True
         }

main = gameApp Tick { model = init, view = view, update = update, title = "Binary101" }

view : Model -> Collage Msg
view model = collage 192 128 (myShapes model)

-- other helper functions
index : List a -> Int -> a -> a
index xs n default = fromMaybe (List.head (List.drop n xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default