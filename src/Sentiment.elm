module Sentiment exposing 
  ( Result
  , trim
  , tokenize
  , analyse
  , analyseWith
  )


import String
import Dict exposing (Dict)
import Dictionary.Afinn as Afinn
import Regex exposing (HowMany(All), regex)


type alias Result = 
  { tokens: List String
  , score: Int
  , words: List String
  , positive: List Int
  , negative: List Int
  , comparative: Float
  }


emptyResult : Result
emptyResult = 
  { tokens = []
  , score = 0
  , words = []
  , positive = []
  , negative = []
  , comparative = 0.0
  }


trim : String -> String
trim word =
  word
  |> Regex.replace All (regex "^\\W+") (\_ -> "")
  |> Regex.replace All (regex "\\W+$") (\_ -> "")


tokenize : String -> List String
tokenize =
  (String.toLower >> String.words >> List.map trim) 


afinn : Dict String Int
afinn = 
  Dict.fromList Afinn.list


singleToken : Dict String Int -> String -> Result -> Result
singleToken sentimentDict token intermediateResult =
  if Dict.member token sentimentDict then
    let
      {score, words, positive, negative} = intermediateResult
      weight = 
        Dict.get token sentimentDict
        |> Maybe.withDefault 0
    in
      { intermediateResult 
        | score = score + weight
        , words = token::words
        , negative = if weight < 0 then weight::negative else negative
        , positive = if weight > 0 then weight::positive else positive
        }
  else
    intermediateResult


analyseWith : (String -> List String) -> Dict String Int -> String -> Result
analyseWith tokenizer inject str =
  let
    token = 
      tokenizer str
    sentimentDict = 
      if Dict.isEmpty inject then
        afinn
      else
        Dict.union inject afinn
    addToken result =
      { result | tokens = token }
    addComparative result =
      let
        len = List.length result.tokens |> toFloat
        score' = toFloat result.score
      in
        { result | comparative = score' / len }
  in
    List.foldr (singleToken sentimentDict) emptyResult token
    |> addToken
    |> addComparative


analyse : String -> Result
analyse =
  analyseWith 
    tokenize
    Dict.empty