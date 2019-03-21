# Computational and Mathematical Linguistics Project #

Participants: Jesse Frohlich and Andrew Wilson

## Ideas ## 

- Build a syntax parser for the constructed language toki pona. Given a
  sentence in toki pona, check for
  - grammaticality (i.e. whether a valid syntax tree exists)
  - ambiguity (i.e. whether multiple such trees exist)

- Find other languages which can be easily parsed
  - Esperanto
  - Ithkuil
  - Lojban

- Syntactic ambiguity
  - Which combinations of syntactic features result in a large number of
    possible syntax trees? I.e. which languages allow for the most syntactic
    puns (as opposed to morphological puns).

- \*Density questions.
  - Can you distinguish poetry from prose from nonesense by analyzing
    compressibility of a passage of text?

## Languages ##
- Haskell (functional, pretty)
- Python (easy-to-code, good for prototyping)
- Other
  - Rust (functional and procedural, safe)
  - Ruby (OOL)
  - Go

## Plan
- Talk to Matilde about the project
- Look into common algorithms for building parse trees
  - [SWI-Prolog parser](https://jan-lope.github.io/Toki_Pona-Parser/)
  - [C# parser](https://github.com/matthewdeanmartin/tokipona.parser/tree/master/TokiPonaTools)
- Read some papers
  - [University of Hawaii](https://www2.hawaii.edu/~chin/661F12/Projects/ztomaszewski.pdf)
- Learn some Haskell
- Learn some toki pona
- Learn some linguistics
  - Is Chompsky normal form computationally desirable?
    - "AdvP la Clause" vs "Clause la AdvP"

## Sources
- [Dictionary](http://tokipona.net/tp/janpije/dictionary.php)

Goals for 2019-03-21
- Finish the lexer
  - tpLex :: String -> [Lexeme]
- Discuss the CFG
