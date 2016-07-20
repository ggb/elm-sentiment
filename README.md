# elm-sentiment

elm-sentiment is an Elm module that uses the [AFINN-111](http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010) wordlist to perform [sentiment analysis](http://en.wikipedia.org/wiki/Sentiment_analysis) on arbitrary blocks of input text.

It is a port of the [Sentiment](https://github.com/thisandagain/sentiment)-module for Node.js.

## Installation

```bash
elm package install ggb/elm-sentiment
```

## Usage

Usage is straightforward: 

```elm
import Sentiment

shortText = """
#StarWars fans are the best kind of people. 
I'm so, so lucky & honored to get to hang 
out with you at Celebration. Thank you for 
being you.
"""

Sentiment.analyse shortText

-- Result:
--
-- { tokens = ["starwars","fans","are","the","best", ... ,"for","being","you"]
-- , score = 12
-- , words = ["best","kind","lucky","honored","thank"]
-- , positive = [3,2,3,2,2]
-- , negative = []
-- , comparative = 0.42857142857142855 
-- }

```

For more advanced usage please take a look at the function-level documentation
and especially at the analyseWith-function.