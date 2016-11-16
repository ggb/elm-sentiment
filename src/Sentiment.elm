module Sentiment exposing 
  ( Result
  , trim
  , tokenize
  , analyse
  , analyseWith
  )

{-| elm-sentiment is an Elm module that uses the [AFINN-111](http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010) wordlist to perform [sentiment analysis](http://en.wikipedia.org/wiki/Sentiment_analysis) on arbitrary blocks of input text. Other wordlists are easy to integrate.

It is inspired by the the [Sentiment](https://github.com/thisandagain/sentiment)-module for Node.js.

**Please note** that a wordlist-based approach for sentiment analysis might not be the best available approach for every (your) application. It is a simple and easy to use solution that does not need training like a Bayes classifier, that might perform better in classifying sentiments.  

@docs Result

# Helper

@docs trim, tokenize

# Analysis

@docs analyse, analyseWith

-}


import String
import Dict exposing (Dict)
import WordList.Afinn as Afinn
import Regex exposing (HowMany(All), regex)


{-| The Result-type describes the information returned by a call to analyse or analyseWith. The struct contains all tokens, the sum of positive and negative scores etc. The value comparative is the overall score divided by the number of words in the input string. 
-}
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


{-| Trim a given input string.

    import Sentiment

    Sentiment.trim "  --- Hello!, "

    -- Hello
-}
trim : String -> String
trim word =
  word
  |> Regex.replace All (regex "^\\W+") (\_ -> "")
  |> Regex.replace All (regex "\\W+$") (\_ -> "")


{-| Split a string into words, turn all words into lowercase and remove everything that is not necessary (e. g. whitespace or special characters).

    import Sentiment

    Sentiment.tokenize " --- Hello, World! :) "

    -- ["hello","world"]
-}
tokenize : String -> List String
tokenize =
  String.toLower 
    >> String.words 
    >> List.map trim 
    >> List.filter ((/=) "") 


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


{-| Analyse a given string and return a struct of type Result. This function expects a dictionary containing a word list, a dictionary with additional sentiment information and a tokenizer function.

    import Sentiment
    import WordList.Afinn as Afinn
    import String
    import Dict

    result = 
      Sentiment.analyseWith 
        Afinn.get
        (Dict.fromList [(":-)",3),(":-|", 0),(":-(", -3)])
        (String.toLower >> String.words)
        "Best movie ever! :-)"

    -- result.score == 6
-}
analyseWith : Dict String Int -> Dict String Int -> (String -> List String) -> String -> Result
analyseWith wordList inject tokenizer str =
  let
    token = 
      tokenizer str
    sentimentDict = 
      if Dict.isEmpty inject then
        wordList
      else
        Dict.union inject wordList
    addToken result =
      { result | tokens = token }
    addComparative result =
      let
        len = List.length result.tokens |> toFloat
        score1 = toFloat result.score
      in
        { result | comparative = score1 / len }
  in
    List.foldr (singleToken sentimentDict) emptyResult token
    |> addToken
    |> addComparative


{-| Analyse a string and return a struct of type Result. The function basically calls analyseWith, but with (good) defaults.

    import Sentiment

    result = Sentiment.analyse "Best movie ever!"

    -- result.score == 3
-}
analyse : String -> Result
analyse =
  analyseWith
    Afinn.get
    Dict.empty
    tokenize