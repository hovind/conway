module Tests exposing (all, iterate)

import Dict
import Expect
import Fuzz
import Main exposing (..)
import Set
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


iterate : (a -> a) -> a -> List a
iterate f x =
    x :: iterate f (f x)


cell : Fuzz.Fuzzer Cell
cell =
    Fuzz.tuple ( Fuzz.intRange 4 6, Fuzz.intRange 4 6 )


world : Fuzz.Fuzzer World
world =
    Fuzz.map Set.fromList <| Fuzz.list cell


all : Test
all =
    describe "A Test Suite"
        [ fuzz world "Block finder" <|
            \world_ ->
                Expect.true "Is a block" <| world_ /= tick world_ || Set.isEmpty world_
        , fuzz world "Period 2 pulsar finder" <|
            \world_ ->
                Expect.true "Is a pulsar" <| world_ /= tick (tick world_) || Set.isEmpty world_
        ]
