-- https://cs1xd3.online/ShowModulePublish?modulePublishId=387ccd78-bdb8-4fc6-83bf-b75de3c8a15e
-- Final 1XD3 math visualizer prototype by Lab 1 Group 2:
--   Owen Gretzinger (gretzino)
--   Hilary He (hez88)
--   Vicky Robinson (robinv2)
--   Louis Doan (doanl7)
--   Victor Moucattash (moucattv)

import Task

-- define colours
bgCol = rgb 65 179 164
highlightCol = rgb 169 229 187
chapterTextCol = black
lessonTextCol = black
buttonCol = rgb 230 230 230

-- info needed by typing app
--   (needed to use typing app slot to import other typing apps)
appTitle = "Binary 0101"
allowTyping model = True
isAnimating model = True
collageWidth = 192
collageHeight = 128

lessonTextList =
  [
  "Normally, we use a base-10 number system called “decimal”. Decimal numbers are made up of different “digits”, which can have a value from 0 to 9. ~In this interactive module, you will learn about the base-2 number system, which is called “binary”. Binary numbers are made up of different “bits”, which can have a value of 0 or 1. ~Click the button above to see examples of decimal and binary numbers from 0-255!"
 ,"The main application for binary is in computers. Since there are only 2 possible values for a bit (0 or 1), computers can store these values much more easily than decimal numbers by using a switch or transistor that can either be on or off. Computers need many transistors to operate, but it is still more simple and less expensive than using decimal numbers. ~Click the toggle in the top right to turn the switch on or off."
 ,"Similar to how computers use transistors being on or off to represent a 1 or 0 bit, it can be beneficial to think of a binary bit as being a lightbulb. ~Click on the switch to turn this lightbulb on or off and change the bit between 0 and 1."
 ,"When working with binary, it’s important to know how to convert to decimal, and vice versa. ~Feel free to play around with 8 lightbulbs and click them to see how it affects the decimal number, or change the decimal number using the arrows and see how it affects the lightbulbs. ~You can also click the swap button to see binary bits instead of lights."
 ,"As we know, a base 10 number has a ones, tens, hundreds column etc. We can deconstruct a number as visualized above. ~In the lightbulb converter you may have noticed numbers under the lights, and this is because you can think of binary the same way except instead of going up by powers of 10 it goes up by powers of 2 (ones, twos, fours, eights column etc.). ~Wait for the animation to finish and it will switch to binary (or click the swap button)."
 ,"Therefore, to convert a binary number into decimal, you can split the bits up, multiply each one by its respective power of 2, and add the resulting decimal numbers to get the binary number’s decimal representation."
 ,"It’s also important to be able to convert a decimal number to binary, although this process is more difficult. ~This is done by repeatedly dividing the decimal number by 2 and taking the remainder as one of the binary representation’s bits. Be careful of the order though, as the last calculated bit is the “most significant” and is the leftmost bit in the final answer. ~Study the visualization and be sure to understand this process."
 ,"Now try to convert this binary number into decimal by yourself using the method of multiplying bits by their respective power of 2. Feel free to go back to review this method. Type your answer into the box (only numbers will register). You can also click the refresh button to get a new question. ~(You can always skip ahead to the next section if you prefer.)"
 ,"Convert this decimal number into binary by yourself using the method of dividing by 2 and taking the remainder. Feel free to go back to review this method. Click on the bits to toggle between 0 and 1. You can also click the refresh button to get a new question. ~(You can always skip ahead to the next section if you prefer.)"
 ,"There is plenty more math with binary that you will learn if you go into computer science. For example, converting between other bases, as well as arithmetic operations such as binary addition, multiplication, and division."
 ,"Speaking of other bases, another base that is commonly used is base 16, called hexadecimal. In hexadecimal, the letters A,B,C,D,E,F represent the numbers 10,11,12,13,14,15, respectively. ~You are probably familiar with hexadecimal being used for colour hex codes, but is also useful for many other purposes, such as memory readouts and encoding braille. It's useful because it holds even more info than decimal, but its base is a power of 2."
 ,"Originally, computers were used primarily as calculators, however, over time computers have needed to be able to hold other information such as text, pictures, audio, and more. ~The most popular way to represent text with binary is the American Standard Code for Information Interchange, or ASCII for short. ~Type text in the left box to see the decimal and binary ASCII representations."
 ,"That's all! Congratulations on graduating Binary 0101! Click on the letter to open it. ~You should now have a visual intuition for basic binary concepts, which will make it easier and much more fun to learn more in-depth concepts if you take computer science courses that cover binary in the future. ~Made by Owen Gretzinger, Hilary He, Vicky Robinson, Louis Doan, and Victor Moucattash for CS1XD3."
  ]

myShapes model =
    [ square 300 |> filled bgCol ]
    ++
    case model.state of
        Menu ->
            [ Gretzino.BinaryMenu.myShapes model.menuModel
                |> group
                |> GraphicSVG.map MenuMsg
            ]
        App ->
            [ square 200 |> filled highlightCol |> move (0,-120)
            , displayLessonText model.lesson
            , getVisuals model
            , chapterTitleHeader model
            , lessonNumHeader model
            -- dont show next lesson button if we are on the last lesson
            , if model.lesson /= -1 + List.length lessonTextList
              then arrow model Next NextLesson
              else group []
            -- dont show previous lesson button if we are on the first lesson
            , if model.lesson /= 0
              then arrow model Back BackLesson |> rotate pi
              else group []
            , homeIcon
            ]

-- system for formatting text in paragraph form
displayLessonText lesson = getLessonText (String.split " " (getAtIndex lessonTextList lesson "")) -26
getLessonText words yPos =
  case words of
    [] -> [] |> group
    word::other -> 
      if String.startsWith "~" word
      then getLessonText ((String.replace "~" "" word)::other) (yPos - 2)
      else
        let thisTxt = List.take (getNumWordsForLine words 0 0) words
            otherTxt = List.drop (getNumWordsForLine words 0 0) words
        in
        group [
        if List.length otherTxt > 0
        then getLessonText otherTxt (yPos - lineSpacing)
        else [] |> group
        , text (String.join " " thisTxt)
            |> size 5 |> filled lessonTextCol |> move (-93,yPos)
        ]
lineSpacing = 6
lineLength = 73
getNumWordsForLine words numToTake charsOnThisLine = 
  case words of
    [] -> numToTake
    word::otherWords ->
      if charsOnThisLine > lineLength
      then numToTake - 1
      else if String.startsWith "~" word
      then numToTake
      else getNumWordsForLine 
           otherWords
           (numToTake+1)
           (charsOnThisLine+String.length word)

chapterTitles = ["0001. What is Binary?", "0010. Binary/Decimal Conversion", "0011. More Binary Applications", "0100. Graduation!"]
displayChapters = (
    List.indexedMap (\j idx ->
    group [ 
        text (getAtIndex chapterTitles idx "")
        |> size 8
        |> filled chapterTextCol
        |> move (0,-2.5)
        ]
        |> move(-75, 10 - 15 * (toFloat idx))
    )
    (List.range 0 (List.length chapterTitles))
    ) |> group

homeIcon = group [ circle 10 |> filled buttonCol
                  , group [
                   roundedRect 10 10 1 |> filled bgCol
                  , roundedRect 11 1.5 0.75 |> filled bgCol
                        |> rotate (-pi/4)
                        |> move (3.5,5)
                  , roundedRect 11 1.5 0.75 |> filled bgCol 
                        |> rotate (pi/4)
                        |> move (-3.5,5)
                  , polygon [(0,8),(5,3),(-5,3)] |> filled bgCol
                  , roundedRect 3 6 1 |> filled (rgb 241 221 223) |> move (0, -3)
                  ] |> move (0, -1) 
                  ] |> scale 0.5
                    |> move (-89.2,58) 
                    |> notifyTap ToMenu

arrow model buttonMsg hoveredButton = group [ 
                  rect 35 128 
                    |> filled black 
                    |> makeTransparent (if model.hovering == Just buttonMsg then 0.5 else 0) 
                    |> move (100,0)
                  , square 15 |> filled buttonCol 
                      |> clip ( group [ roundedRect 10 3 1.5 |> ghost
                                          |> rotate (-pi/4)
                                          |> move (2,2.7)
                                      , roundedRect 10 3 1.5 |> ghost
                                          |> rotate (pi/4)
                                          |> move (2,-2.7)
                                      ])
                                  |> move (87.5, 0) 
                                  |> makeTransparent (if model.hovering == Just buttonMsg then 1 else 0.7) 
                 ] |> notifyEnter (Hovered buttonMsg) |> notifyLeave Leave |> notifyTap hoveredButton 

chapterTitleHeader model =
  let num = if model.lesson == 12 then 3 else if model.lesson < 3 then 0 else if model.lesson < 9 then 1 else 2
  in text (getAtIndex chapterTitles num "")
       |> centered
       |> size 5
       |> filled black
       |> move (0,57)
lessonNumHeader model = 
  text (String.fromInt (model.lesson+1) ++ "/" ++ String.fromInt (List.length lessonTextList))
    |> alignRight
    |> size 5
    |> filled black
    |> move (93,57)

-- display the animations and interactive components
getVisuals model = 
  case model.lesson + 1 of
    1 -> group [ Gretzino.RandBinDec.myShapes model.randBinDecModel
                   |> group
                   |> GraphicSVG.map RandBinDecMsg
               , group [ tipText "click this or press" |> move (0,3)
                       , tipText "the right arrow key to" |> move (0,0)
                       , tipText "go to the next lesson" |> move (0,-3)
                       ] |> rotate 0.2
                         |> move (62,-3)
               , tipArrow |> rotate (pi/4) |> move (68,-6)
               , group [ tipText "click this or press" |> move (0,3)
                       , tipText "escape to go to" |> move (0,0)
                       , tipText "the main menu" |> move (0,-3)
                       ] |> rotate -0.2
                         |> move (-62,53)
               , tipArrow |> scaleX -1 |> rotate (-pi/4) |> move (-68,50)
               ]
    2 -> Gretzino.Switch.myShapes model.switchModel
           |> group
           |> GraphicSVG.map SwitchMsg
           |> move (0,5)
    3 -> Gretzino.SingleLight.myShapes model.singleLightModel
           |> group
           |> GraphicSVG.map SingleLightMsg
           |> move (0,15)
    4 -> Gretzino.LightVisualization.myShapes model.eightLightsModel
           |> group
           |> GraphicSVG.map EightLightsMsg
    5 -> group [ whiteBG
               , Gretzino.NewVisualizerColumnAnimation.myShapes model.columnAnimModel
                     |> group
                     |> GraphicSVG.map ColumnAnimMsg
                     |> move (0,4)]
    6 -> Gretzino.NewBin2DecVisual.myShapes model.bin2DecVisualModel
           |> group
           |> GraphicSVG.map Bin2DecVisualMsg
    7 -> group [ whiteBG
               , Gretzino.NewDecimalToBinaryAnimation.myShapes model.dec2BinVisualModel
                     |> group
                     |> GraphicSVG.map Dec2BinVisualMsg
                     |> scale 0.6
                     |> move (0,20)]
    8 -> Gretzino.Bin2DecPractice.myShapes model.bin2decPracticeModel
           |> group
           |> GraphicSVG.map Bin2DecPracticeMsg
           |> move (0,-5)
    9 -> Gretzino.Decimal2BinaryPractice.myShapes model.decimal2BinaryPracticeModel
           |> group
           |> GraphicSVG.map Decimal2BinaryPractice
           |> move (0,-5)
    10 -> Gretzino.ExtraApps.myShapes model.extraAppsModel
           |> group
           |> GraphicSVG.map ExtraAppsMsg
    11 -> group [ whiteBG
               , Gretzino.HexCol.myShapes model.hexColModel
                     |> group
                     |> GraphicSVG.map HexColMsg
                     ]
    12 -> Gretzino.ASCII.myShapes model.asciiModel
           |> group
           |> GraphicSVG.map ASCIIMsg
           |> move (0,5)
    13 -> Gretzino.Credits.myShapes model.creditsModel
           |> group
           |> GraphicSVG.map CreditsMsg
           |> move (0,-2)
    otherwise -> [] |> group
tipText txt = text txt |> centered |> size 4 |> filled black
tipArrow = 
  group [ curve (0,0) [Pull (0,0) (0,0), Pull (0,-10) (10,-10) ]
            |> outlined (solid 0.5) black
        , rect 5 0.5 |> filled black |> rotate (pi/4) |> move (8,-11.8)
        , rect 5 0.5 |> filled black |> rotate (-pi/4) |> move (8,-8.2)
  ]
whiteBG = roundedRect 188 80 4 |> filled white |> move (0,22)

changeLessonNum current dir =
  let new = current + dir
  in
  if new >= List.length lessonTextList || new < 0
  then current
  else new

type State = Menu | App
type Button = Back | Next

type Msg = Tick Float
         -- main app messages
         | ToMenu
         | Hovered Button
         | Leave
         | ResetImports
         | NextLesson
         | BackLesson
         -- typing app messages
         | KeyDown String
         | KeyUp String
         | NoOp
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         -- imported module messages
         | ArrowKeyMsg Gretzino.GetArrowKeys.Msg
         | MenuMsg Gretzino.BinaryMenu.Msg
         | RandBinDecMsg Gretzino.RandBinDec.Msg
         | ColumnAnimMsg Gretzino.NewVisualizerColumnAnimation.Msg
         | EightLightsMsg Gretzino.LightVisualization.Msg
         | Decimal2BinaryPractice Gretzino.Decimal2BinaryPractice.Msg
         | Bin2DecPracticeMsg Gretzino.Bin2DecPractice.Msg
         | SwitchMsg Gretzino.Switch.Msg
         | SingleLightMsg Gretzino.SingleLight.Msg
         | Bin2DecVisualMsg Gretzino.NewBin2DecVisual.Msg
         | Dec2BinVisualMsg Gretzino.NewDecimalToBinaryAnimation.Msg
         | HexColMsg Gretzino.HexCol.Msg
         | ASCIIMsg Gretzino.ASCII.Msg
         | ExtraAppsMsg Gretzino.ExtraApps.Msg
         | CreditsMsg Gretzino.Credits.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ({ model 
            | time = t
            -- update time based animation visuals
            , columnAnimModel = 
                let 
                  (newModel, newCmd) = Gretzino.NewVisualizerColumnAnimation.update (Gretzino.NewVisualizerColumnAnimation.Tick (t - model.lessonStartTime)) model.columnAnimModel
                in
                  newModel
            , bin2DecVisualModel = 
                let 
                  (newModel, newCmd) = Gretzino.NewBin2DecVisual.update (Gretzino.NewBin2DecVisual.Tick (t - model.lessonStartTime)) model.bin2DecVisualModel
                in
                  newModel
            , dec2BinVisualModel = 
                let 
                  (newModel, newCmd) = Gretzino.NewDecimalToBinaryAnimation.update (Gretzino.NewDecimalToBinaryAnimation.Tick (t - model.lessonStartTime)) model.dec2BinVisualModel
                in
                  newModel   
            , extraAppsModel = 
                let 
                  (newModel, newCmd) = Gretzino.ExtraApps.update (Gretzino.ExtraApps.Tick (t - model.lessonStartTime)) model.extraAppsModel
                in
                  newModel   
            , creditsModel = 
                let 
                  (newModel, newCmd) = Gretzino.Credits.update (Gretzino.Credits.Tick (t - model.lessonStartTime)) model.creditsModel
                in
                  newModel
            }, Cmd.none)
        -- return to the menu
        ToMenu -> ({ model | state = Menu }, Cmd.none)
        
        --button animations
        Hovered button -> ( { model | hovering = Just button }, Cmd.none )
        Leave -> ( { model | hovering = Nothing }, Cmd.none )
        
        -- reset imported modules
        --   (for example to reset time based animations
        --    so that they play from the beginning if you
        --    go to a different lesson then later return)
        ResetImports ->
          ({ model 
           | lessonStartTime = model.time
           , menuModel = Gretzino.BinaryMenu.init
           , columnAnimModel = Gretzino.NewVisualizerColumnAnimation.init
           , bin2DecVisualModel = Gretzino.NewBin2DecVisual.init
           , dec2BinVisualModel = Gretzino.NewDecimalToBinaryAnimation.init
           }, Cmd.none)
        
        -- lesson navigation
        ArrowKeyMsg keyMsg -> (model, case keyMsg of
                         Gretzino.GetArrowKeys.RightArrowPressed -> 
                           -- fix bug; credits visual would keep resetting
                           -- if the right arrow key was held down
                           if model.lesson < -1 + List.length lessonTextList
                           then run NextLesson
                           else Cmd.none
                         Gretzino.GetArrowKeys.LeftArrowPressed -> run BackLesson
                         otherwise -> Cmd.none)
        NextLesson -> ({ model 
                       | lesson = changeLessonNum model.lesson 1
                       }, run ResetImports)
        BackLesson -> ({ model 
                       | lesson = changeLessonNum model.lesson -1
                       }, run ResetImports)
        
        -- typing app messages
        KeyDown code -> 
          case code of
          "Escape" -> (model,run ToMenu)
          -- send key down messages to imported typing apps
          otherwise ->
            let 
              (arrowModel, arrowCmd) = Gretzino.GetArrowKeys.update (Gretzino.GetArrowKeys.KeyDown code) model.getArrowKeyModel
              (bin2decModel, bin2decCmd) =
                if model.lesson == 7
                then Gretzino.Bin2DecPractice.update (Gretzino.Bin2DecPractice.KeyDown code) model.bin2decPracticeModel
                else (model.bin2decPracticeModel, Cmd.none)
              (newAsciiModel, asciiCmd) = 
                if model.lesson == 11
                then Gretzino.ASCII.update (Gretzino.ASCII.KeyDown code) model.asciiModel
                else (model.asciiModel, Cmd.none)
            in
              ({ model 
              | getArrowKeyModel = arrowModel 
              , bin2decPracticeModel = bin2decModel
              , asciiModel = newAsciiModel
              }, Cmd.batch [ Cmd.map ArrowKeyMsg arrowCmd
                           , Cmd.map Bin2DecPracticeMsg bin2decCmd
                           , Cmd.map ASCIIMsg asciiCmd
                           ])
        KeyUp _ -> (model,Cmd.none)
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
        
        -- imported module messages
        MenuMsg menuMsg -> case menuMsg of
          Gretzino.BinaryMenu.ToApp -> 
              ({ model 
               | state = App
               , lesson = 0
               }, Cmd.batch [ run ResetImports
                            , Cmd.map Bin2DecPracticeMsg (run Gretzino.Bin2DecPractice.ChangeQuestion)
                            , Cmd.map Decimal2BinaryPractice (run Gretzino.Decimal2BinaryPractice.ChangeQuestion)
                            ])
          otherwise -> ({ model | menuModel = Gretzino.BinaryMenu.update menuMsg model.menuModel }, Cmd.none)
        RandBinDecMsg randBinDecMsg -> 
          let 
            (newModel, newCmd) = Gretzino.RandBinDec.update randBinDecMsg model.randBinDecModel
          in
            ({ model | randBinDecModel = newModel }, Cmd.map RandBinDecMsg newCmd)
        ColumnAnimMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.NewVisualizerColumnAnimation.update imsg model.columnAnimModel
          in
            ({ model | columnAnimModel = newModel }, Cmd.map ColumnAnimMsg newCmd)
        EightLightsMsg imsg -> 
          ({ model | eightLightsModel = Gretzino.LightVisualization.update imsg model.eightLightsModel }, Cmd.none)
        Decimal2BinaryPractice imsg -> 
          let 
            (newModel, newCmd) = Gretzino.Decimal2BinaryPractice.update imsg model.decimal2BinaryPracticeModel
          in
            ({ model | decimal2BinaryPracticeModel = newModel }, Cmd.map Decimal2BinaryPractice newCmd)
        SwitchMsg imsg -> 
          ({ model | switchModel = Gretzino.Switch.update imsg model.switchModel }, Cmd.none)
        SingleLightMsg imsg -> 
          ({ model | singleLightModel = Gretzino.SingleLight.update imsg model.singleLightModel }, Cmd.none)
        Bin2DecVisualMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.NewBin2DecVisual.update imsg model.bin2DecVisualModel
          in
            ({ model | bin2DecVisualModel = newModel }, Cmd.map Bin2DecVisualMsg newCmd)
        Dec2BinVisualMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.NewDecimalToBinaryAnimation.update imsg model.dec2BinVisualModel
          in
            ({ model | dec2BinVisualModel = newModel }, Cmd.map Dec2BinVisualMsg newCmd)
        HexColMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.HexCol.update imsg model.hexColModel
          in
            ({ model | hexColModel = newModel }, Cmd.map HexColMsg newCmd)
        Bin2DecPracticeMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.Bin2DecPractice.update imsg model.bin2decPracticeModel
          in
            ({ model | bin2decPracticeModel = newModel }, Cmd.map Bin2DecPracticeMsg newCmd)
        ASCIIMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.ASCII.update imsg model.asciiModel
          in
            ({ model | asciiModel = newModel }, Cmd.map ASCIIMsg newCmd)
        ExtraAppsMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.ExtraApps.update imsg model.extraAppsModel
          in
            ({ model | extraAppsModel = newModel }, Cmd.map ExtraAppsMsg newCmd)
        CreditsMsg imsg -> 
          let 
            (newModel, newCmd) = Gretzino.Credits.update imsg model.creditsModel
          in
            ({ model | creditsModel = newModel }, Cmd.map CreditsMsg newCmd)


type alias Model =
    { time : Float
    , state : State
    , window : Window
    , lesson : Int
    , lessonStartTime : Float
    , hovering : Maybe Button
    , getArrowKeyModel : Gretzino.GetArrowKeys.Model
    , menuModel : Gretzino.BinaryMenu.Model
    , randBinDecModel : Gretzino.RandBinDec.Model
    , columnAnimModel : Gretzino.NewVisualizerColumnAnimation.Model
    , eightLightsModel : Gretzino.LightVisualization.Model
    , decimal2BinaryPracticeModel : Gretzino.Decimal2BinaryPractice.Model
    , switchModel : Gretzino.Switch.Model
    , singleLightModel : Gretzino.SingleLight.Model
    , bin2DecVisualModel : Gretzino.NewBin2DecVisual.Model
    , dec2BinVisualModel : Gretzino.NewDecimalToBinaryAnimation.Model
    , hexColModel : Gretzino.HexCol.Model
    , bin2decPracticeModel : Gretzino.Bin2DecPractice.Model
    , asciiModel : Gretzino.ASCII.Model
    , extraAppsModel : Gretzino.ExtraApps.Model
    , creditsModel : Gretzino.Credits.Model
    }

init : Model
init = { time = 0 
       , state = Menu
       , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
       , lesson = 0
       , lessonStartTime = 0
       , hovering = Nothing
       , getArrowKeyModel = Gretzino.GetArrowKeys.init
       , menuModel = Gretzino.BinaryMenu.init
       , randBinDecModel = Gretzino.RandBinDec.init
       , columnAnimModel = Gretzino.NewVisualizerColumnAnimation.init
       , eightLightsModel = Gretzino.LightVisualization.init
       , decimal2BinaryPracticeModel = Gretzino.Decimal2BinaryPractice.init
       , switchModel = Gretzino.Switch.init
       , singleLightModel = Gretzino.SingleLight.init
       , bin2DecVisualModel = Gretzino.NewBin2DecVisual.init
       , dec2BinVisualModel = Gretzino.NewDecimalToBinaryAnimation.init
       , hexColModel = Gretzino.HexCol.init
       , bin2decPracticeModel = Gretzino.Bin2DecPractice.init
       , asciiModel = Gretzino.ASCII.init
       , extraAppsModel = Gretzino.ExtraApps.init
       , creditsModel = Gretzino.Credits.init
       }

view model = collage 192 128 (myShapes model)

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
