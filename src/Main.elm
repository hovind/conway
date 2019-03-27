module Main exposing (main)

import Browser
import Dict
import Element
import Element.Background
import Element.Border
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Random
import Set
import Time



---- MODEL ----


type alias Cell =
    ( Int, Int )


add : Cell -> Cell -> Cell
add ( a1, a2 ) ( b1, b2 ) =
    ( a1 + b1, a2 + b2 )


directions : Set.Set Cell
directions =
    Set.fromList [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]


neighbouringCells : Cell -> Set.Set Cell
neighbouringCells cell =
    Set.map (add cell) directions


alive : World -> Cell -> Bool
alive world cell =
    Dict.get cell world == Just ()


neighbours : World -> Cell -> Set.Set Cell
neighbours world cell =
    neighbouringCells cell
        |> Set.filter (alive world)


deadCells : World -> Set.Set Cell
deadCells world =
    Dict.keys world
        |> List.map (neighbouringCells >> Set.toList)
        |> List.concat
        |> Set.fromList
        |> Set.filter (alive world >> not)


newCells : World -> Set.Set Cell
newCells world =
    deadCells world
        |> Set.filter (\deadCell -> Set.size (neighbours world deadCell) == 3)


remainingCells : World -> Set.Set Cell
remainingCells world =
    Dict.keys world
        |> Set.fromList
        |> Set.filter (\cell -> Set.size (neighbours world cell) == 2 || Set.size (neighbours world cell) == 3)


toWorld : Set.Set Cell -> World
toWorld =
    Set.foldr (\element accumulator -> Dict.insert element () accumulator) Dict.empty


type alias World =
    Dict.Dict Cell ()


type alias Model =
    World


init : ( Model, Cmd Msg )
init =
    ( toWorld <| Set.fromList [ ( 3, 4 ), ( 4, 4 ), ( 5, 4 ) ]
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix


tick : World -> World
tick world =
    toWorld <| Set.union (newCells world) (remainingCells world)


update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick _) world =
    ( tick world, Cmd.none )


dimension : Int
dimension =
    9


showBool : Bool -> Element.Element msg
showBool bool =
    Element.el
        [ Element.width <| Element.px 40
        , Element.height <| Element.px 40
        , Element.Border.color <|
            Element.rgb255 127 127 127
        , Element.Border.width 2
        , Element.Background.color <|
            if bool then
                Element.rgb255 0 0 0

            else
                Element.rgb255 255 255 255
        ]
        Element.none



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.row [ Element.centerX, Element.centerY ] <|
            List.map (Element.column [] << List.map showBool) <|
                List.indexedMap (\i column -> List.indexedMap (\j () -> Dict.get ( i, j ) model == Just ()) column) <|
                    List.repeat dimension (List.repeat dimension ())



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Time.every 500 Tick
        }
