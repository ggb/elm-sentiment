module Tests exposing (..)

import Test exposing (..)
import Expect
import Fixtures exposing (..)
import Sentiment exposing (..)
import Dict
import WordList.Afinn as Afinn


testHelper i (result, expected) = 
    test ("Test " ++ toString i) <|
        \() ->
            Expect.equal result expected |> Debug.log ""


ex1 = analyse "Hey you worthless scumbag"
ex2 = analyse "This is so cool"
ex3 = analyse "self-deluded"
ex4 = analyse "constructor"
ex5 = analyseWith Afinn.get (Dict.fromList [("cool", 100)]) tokenize "This is so cool"
ex6 = analyse corpusText


tests = 
    -- ex1
    [ (-6, ex1.score |> toFloat)
    , (-1.5, ex1.comparative)
    , (4, (List.length ex1.tokens |> toFloat))
    , (2, (List.length ex1.words |> toFloat))
    -- ex2
    , (1, ex2.score |> toFloat)
    , (0.25, ex2.comparative)
    , (4, (List.length ex2.tokens |> toFloat))
    , (1, (List.length ex2.words |> toFloat))
    -- ex3
    , (-2, ex3.score |> toFloat)
    , (-2, ex3.comparative)
    , (1, (List.length ex3.tokens |> toFloat))
    , (1, (List.length ex3.words |> toFloat))
    -- ex4
    , (0, ex4.score |> toFloat)
    , (0, ex4.comparative)
    , (1, (List.length ex4.tokens |> toFloat))
    , (0, (List.length ex4.words |> toFloat))
    -- ex5
    , (100, ex5.score |> toFloat)
    , (25, ex5.comparative)
    , (4, (List.length ex5.tokens |> toFloat))
    , (1, (List.length ex5.words |> toFloat))
    -- ex6
    , (-7, ex6.score |> toFloat)
    -- , (0 ex6.comparative)
    , (1416, (List.length ex6.tokens |> toFloat))
    , (72, (List.length ex6.words |> toFloat))
    ] |> List.indexedMap testHelper


all : Test
all =
    describe "Sample Test Suite" tests