-- https://cs1xd3.online/ShowModulePublish?modulePublishId=de3a138e-bc38-49ab-8da7-308e4189912f
-- app main menu

-- define colours
highlightCol = rgb 169 229 187
logoTextCol = black
chapterTextCol = black
buttonTextCol = rgb 100 100 100
buttonCol = rgb 230 230 230
shadowCol = rgb 13 145 121

myShapes model = 
  [ logo |> move (-23, 30) |> repaint shadowCol -- shadow
  , logo |> move (-25, 32)
  , displayChapters
  , beginButton model
  ]

chapterTitles = ["0001. What is Binary?", "0010. Binary/Decimal Conversion", "0011. More Binary Applications"]
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

beginButton model = 
    group [
      roundedRect 50 15 7.5|> filled shadowCol |> move (2,-2)
      ,
      roundedRect 50 15 7.5|> filled (if model.hovered then highlightCol else buttonCol)
      ,
      text "Begin" |> size 10 |> centered |> filled buttonTextCol |> move (0,-3)
    ] 
    |> scale (if model.hovered then 1.1 else 1)
    |> move (0,-45)
    |> notifyEnter StartHover
    |> notifyLeave StopHover
    |> notifyTap ToApp

type Msg = Tick Float GetKeyState
         | ToApp
         | StartHover
         | StopHover

update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }
        ToApp -> model
        StartHover -> { model | hovered = True }
        StopHover  -> { model | hovered = False }

type alias Model =
    { time : Float
    , hovered : Bool
    }

init : Model
init = { time = 0
       , hovered = False
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Binary 101" }

view model = collage 192 128 (myShapes model)

-- title graphics
binary = group
 [
    b |> filled logoTextCol
    ,
    bHole |> filled highlightCol
    ,
    bHole |> filled highlightCol |> move (0,-18)
    ,
    i |> filled logoTextCol
    ,
    n |> filled logoTextCol
    ,
    a |> filled logoTextCol
    ,
    aHole |> filled highlightCol
    ,
    r |> filled logoTextCol
    ,
    rHole |> filled highlightCol
    ,
    y |> filled logoTextCol
 ]
num = group
  [
    zero |> filled logoTextCol |> move (-30,-50)
    ,
    zeroHole |> filled highlightCol |> move (-30,-50)
    ,
    one |> filled logoTextCol |> move (20,-50)
    ,
    zero |> filled logoTextCol |> move (20,-50)
    ,
    zeroHole |> filled highlightCol |> move (20,-50)
    ,
    one |> filled logoTextCol |> move (70,-50)
  ]
logo = group
  [ rect 150 15 |> filled highlightCol |> move (25,8)
  , binary |> scale 0.5
  , num |> scale 0.5 |> move (48, 25)
  ]
b = curve (-79.86,38.039) [Pull (-79.50,17.307) (-79.14,-3.425),Pull (-55.48,-6.659) (-54.62,4.5070),Pull (-51.74,15.138) (-59.67,18.569),Pull (-51.32,23.439) (-54.98,31.909),Pull (-58.84,41.994) (-79.50,37.678)]
bHole = curve (-69.04,30.828) [Pull (-69.04,26.140) (-69.04,21.453),Pull (-64.12,20.797) (-64,26.140),Pull (-63.52,31.484) (-69.04,30.828)]
i = curve (-51.74,37.678) [Pull (-46.33,37.498) (-40.92,37.318),Pull (-40.74,16.585) (-40.56,-4.146),Pull (-45.97,-4.146) (-51.38,-4.146),Pull (-51.56,16.766) (-51.74,37.678)]
n = curve (-37.67,-3.425) [Pull (-32.63,-3.425) (-27.58,-3.425),Pull (-27.94,5.7690) (-28.30,14.963),Pull (-24.51,4.6873) (-20.73,-5.588),Pull (-16.22,-5.228) (-11.71,-4.867),Pull (-11.89,16.766) (-12.07,38.4),Pull (-16.76,38.4) (-21.45,38.4),Pull (-20.91,27.222) (-20.37,16.045),Pull (-23.97,26.140) (-27.58,36.236),Pull (-32.63,36.236) (-37.67,36.236),Pull (-37.67,16.405) (-37.67,-3.425)]
a = curve (-3.785,38.039) [Pull (-6.490,17.487) (-9.194,-3.064),Pull (-3.966,-3.245) (1.2619,-3.425),Pull (1.2619,-1.081) (1.2619,1.2619),Pull (4.1464,1.2619) (7.0309,1.2619),Pull (7.2112,-1.983) (7.3915,-5.228),Pull (13.160,-5.228) (18.929,-5.228),Pull (15.323,16.225) (11.718,37.678),Pull (3.9661,37.678) (-3.785,37.678)]
aHole = curve (4.1464,27.943) [Pull (3.0647,18.929) (1.9830,9.9154),Pull (3.9661,10.095) (5.9492,10.276),Pull (5.0478,19.109) (4.1464,27.943)]
r = curve (20.371,38.4) [Pull (20.371,17.847) (20.371,-2.704),Pull (25.780,-2.523) (31.188,-2.343),Pull (31.008,4.1464) (30.828,10.636),Pull (31.369,10.456) (31.909,10.276),Pull (34.073,2.5239) (36.236,-5.228),Pull (41.645,-5.047) (47.053,-4.867),Pull (44.169,4.1464) (41.284,13.160),Pull (46.208,16.470) (46.332,25.780),Pull (44.763,37.850) (34.794,39.121),Pull (27.583,39.780) (20.371,38.039)]
rHole = curve (30.828,30.107) [Pull (30.828,23.797) (30.828,17.487),Pull (34.791,16.952) (35.154,23.616),Pull (34.791,30.461) (30.828,30.107)]
y = curve (46.692,36.957) [Pull (51.380,21.092) (56.067,5.2281),Pull (56.067,0.1802) (56.067,-4.867),Pull (61.656,-4.687) (67.245,-4.507),Pull (66.704,0) (66.163,4.5070),Pull (70.850,21.453) (75.538,38.4),Pull (70.129,38.4) (64.721,38.4),Pull (63.098,29.025) (61.476,19.650),Pull (60.033,28.304) (58.591,36.957),Pull (52.822,36.957) (47.053,36.957)]
one = curve (9.1943,37.678) [Pull (12.619,37.318) (16.045,36.957),Pull (16.045,16.405) (16.045,-4.146),Pull (10.636,-3.966) (5.2281,-3.785),Pull (5.2281,10.095) (5.2281,23.977),Pull (2.7042,22.715) (0.1802,21.453),Pull (-1.442,26.681) (-3.064,31.909),Pull (3.0647,34.794) (9.1943,37.678)]
zero = curve (33.352,38.760) [Pull (22.061,38.329) (20.371,24.698),Pull (18.335,7.3380) (24.698,-1.622),Pull (29.025,-6.005) (33.352,-5.588),Pull (42.661,-5.521) (45.971,4.1464),Pull (52.261,38.253) (33.352,38.760)]
zeroHole = curve (31.549,27.222) [Pull (31.549,16.405) (31.549,5.5887),Pull (33.532,1.9887) (35.515,5.5887),Pull (35.515,16.225) (35.515,26.861),Pull (33.532,31.061) (31.549,26.861)]

-- other helper functions
getAtIndex : List a -> Int -> a -> a
getAtIndex xs p default = fromMaybe (List.head (List.drop p xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default
