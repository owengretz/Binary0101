-- https://cs1xd3.online/ShowModulePublish?modulePublishId=3d2d89b2-3128-48db-8039-b1e025b35653
-- Generates a random number to be used for the 
-- binary to decimal practice module
-- because the typing app doesn't import Random

myShapes model = [
        [
        roundedRect 120 15 5
            |> filled black
            |> makeTransparent 0.3
        ,
        text "Generate Random Number" 
            |> centered 
            |> size 10
            |> filled black
            |> move (0,-3)
        ] |> group
          |> notifyTap GenerateRandomNumber
        ,
        text (String.fromInt model.num)
            |> centered
            |> size 10
            |> filled black
            |> move (0,20) 
    ]


type Msg
    = Tick Float GetKeyState
    | GenerateRandomNumber
    | NewRandomNumber Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ -> 
            ( { model | time = t }, Cmd.none )
        GenerateRandomNumber ->
            ( model, Random.generate NewRandomNumber (Random.int 0 255) )
        NewRandomNumber number ->
            ( {model | num = number }, Cmd.none )

type alias Model =
    { time : Float
    , num : Int
    }

init : Model
init =
    { time = 0
    , num = 0
    }

--view : Model -> Html Msg
view model = collage 192 128 (myShapes model) 

main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> (init, Cmd.none)
        , view = \model -> { title = "Title", body = view model }
        , update = update
        , subscriptions = \_ -> Sub.none
        }