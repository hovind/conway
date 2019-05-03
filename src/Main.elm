module Main exposing (Cell, Model, Msg(..), World, add, alive, deadCells, dimension, directions, init, main, neighbouringCells, neighbours, newCells, remainingCells, showBool, tick, update, view)

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


directions : World
directions =
    Set.fromList [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]


neighbouringCells : Cell -> World
neighbouringCells cell =
    Set.map (add cell) directions


alive : World -> Cell -> Bool
alive world cell =
    Set.member cell world


neighbours : World -> Cell -> World
neighbours world cell =
    neighbouringCells cell
        |> Set.filter (alive world)


deadCells : World -> World
deadCells world =
    Set.toList world
        |> List.map (neighbouringCells >> Set.toList)
        |> List.concat
        |> Set.fromList
        |> Set.filter (alive world >> not)


newCells : World -> Set.Set Cell
newCells world =
    deadCells world
        |> Set.filter (\deadCell -> Set.size (neighbours world deadCell) == 3)


remainingCells : World -> World
remainingCells world =
    Set.filter (\cell -> Set.size (neighbours world cell) == 2 || Set.size (neighbours world cell) == 3) world


type alias World =
    Set.Set Cell


type alias Model =
    World


init : ( Model, Cmd Msg )
init =
    ( Set.fromList [ ( 5, 4 ), ( 5, 5 ), ( 6, 4 ), ( 6, 5 ) ]
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix


tick : World -> World
tick world =
    Set.union (newCells world) (remainingCells world)


update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick _) world =
    ( tick world, Cmd.none )


dimension : Int
dimension =
    20


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
                List.indexedMap (\i column -> List.indexedMap (\j () -> Set.member ( i, j ) model) column) <|
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
