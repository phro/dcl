~~ Lexer for Toki Pona ~~
Authors: Jesse Frohlich & Andrew Wilson
License: GPL3

> import Data.List.Split
> import Data.Char
> import Data.Maybe
> import Control.Monad
> import Data.List

A rough classification of words in Toki Pona

< data Lexeme = Content String     -- Noun, verb, modifier
<             | Borrow String      -- (always modifiers)
<             | Preposition String -- e, tawa, kepeken, lon, poka, &c.
<             | Seperator String   -- ",":".":"pi":[]:li
<             | Nonsense String    -- to collect errors
<             deriving (Show)

Lexer based on the lex function. This splits the string into words and
punctuation, each as their own string. Note that lex is designed to parse
haskell source code; the fact that it works for us is luck, and if something
goes wrong, it may not be a bad idea to switch to lexer' below.

> lexer :: String -> [Symbol]
> lexer [] = []
> lexer s  = Terminal n : lexer s'
>   where [(n,s')] = lex s


Before discovering the lex function in Data.Char, we developed the function
lexer':

> cutAt :: Char -> String -> [String]
> cutAt a s = splitOn [a] s
>
> trimSeperator :: String -> [String]
> trimSeperator s = if not . isAlpha $ c
>                      then init s : [[c]]
>                      else [s]
>                        where c = last s
>
> lexer' :: String -> [String]
> lexer' = join . (map trimSeperator) . (cutAt ' ')

Context Free Grammar sturcture. Should contain nonterminals, terminals,
production rules, and a start symbol.

> data Tree node leaf = Node node (Tree node leaf) (Tree node leaf)
>                     | Leaf leaf
>                     deriving (Eq,Show)
>
>{- > data Terminal =  -}

> data Symbol = Start
>             | Content
>             | SubjectPhrase
>             | Predicate
>             | Preposition
>             | PrepositionPhrase
>             | PrepositionCluster
>          -- | VerbPhrase
>             | Li -- Only produces: Terminal "li" 
>             | PiPhrase
>             | Pi
>             | Terminal String
>             deriving (Show,Eq)
>          -- | Terminal String
>          -- | Missing: taso as a particle, la as a context delimiter

< data Terminal = Terminal String deriving (Show,Eq)

Production rules:

> type RuleName = String

> data ProductionRule = ProductionRule Symbol
>   (Either (Symbol,Symbol) Symbol)
>   deriving (Show)

Playground

> grammar =
>   [ProductionRule Start $ Left (SubjectPhrase,Predicate)
>   ,ProductionRule SubjectPhrase $ Right (Terminal "mi")
>   ,ProductionRule SubjectPhrase $ Right (Terminal "sina")
>   ,ProductionRule SubjectPhrase $ Left (Content,Li)
>   ,ProductionRule Li $ Right (Terminal "li")
> 

Content -> Content "pi" Content

>   ,ProductionRule Content $ Left (Content,Content)
>   ,ProductionRule Content $ Left (Content,PiPhrase)
>   ,ProductionRule PiPhrase $ Left (Pi,Content)

>   ,ProductionRule Predicate $ Left (Content,Content)
>   ,ProductionRule Predicate $ Left (Content,PrepositionCluster)

>   ,ProductionRule PrepositionCluster $ Left
>   (PrepositionPhrase,PrepositionCluster)
>   ,ProductionRule PrepositionCluster $ Left
>   (PrepositionPhrase,PrepositionCluster)
 
PrepositionCluster -> PrepositionPhrase

>   ,ProductionRule PrepositionCluster $ Left (Preposition,Content)
>   ,ProductionRule PrepositionPhrase $ Left (Preposition,Content)

Prepositions 

>   ,ProductionRule Preposition $ Right (Terminal "e")
>   ,ProductionRule Preposition $ Right (Terminal "kepeken")
>   ,ProductionRule Preposition $ Right (Terminal "lon")
>   ,ProductionRule Preposition $ Right (Terminal "tawa")
>   ,ProductionRule Preposition $ Right (Terminal "tan")
>   ,ProductionRule Preposition $ Right (Terminal "sama")
>   ,ProductionRule Preposition $ Right (Terminal "poka")

Content Words

>   ,ProductionRule Content   $ Right (Terminal "akesi")
>   ,ProductionRule Predicate $ Right (Terminal "akesi")
>   ,ProductionRule Content   $ Right (Terminal "ala")
>   ,ProductionRule Predicate $ Right (Terminal "ala")
>   ,ProductionRule Content   $ Right (Terminal "alasa")
>   ,ProductionRule Predicate $ Right (Terminal "alasa")
>   ,ProductionRule Content   $ Right (Terminal "ali")
>   ,ProductionRule Predicate $ Right (Terminal "ali")
>   ,ProductionRule Content   $ Right (Terminal "ale")
>   ,ProductionRule Predicate $ Right (Terminal "ale")
>   ,ProductionRule Content   $ Right (Terminal "anpa")
>   ,ProductionRule Predicate $ Right (Terminal "anpa")
>   ,ProductionRule Content   $ Right (Terminal "ante")
>   ,ProductionRule Predicate $ Right (Terminal "ante")
>   ,ProductionRule Content   $ Right (Terminal "awen")
>   ,ProductionRule Predicate $ Right (Terminal "awen")
>   ,ProductionRule Content   $ Right (Terminal "esun")
>   ,ProductionRule Predicate $ Right (Terminal "esun")
>   ,ProductionRule Content   $ Right (Terminal "ijo")
>   ,ProductionRule Predicate $ Right (Terminal "ijo")
>   ,ProductionRule Content   $ Right (Terminal "ike")
>   ,ProductionRule Predicate $ Right (Terminal "ike")
>   ,ProductionRule Content   $ Right (Terminal "ilo")
>   ,ProductionRule Predicate $ Right (Terminal "ilo")
>   ,ProductionRule Content   $ Right (Terminal "insa")
>   ,ProductionRule Predicate $ Right (Terminal "insa")
>   ,ProductionRule Content   $ Right (Terminal "jaki")
>   ,ProductionRule Predicate $ Right (Terminal "jaki")
>   ,ProductionRule Content   $ Right (Terminal "jan")
>   ,ProductionRule Predicate $ Right (Terminal "jan")
>   ,ProductionRule Content   $ Right (Terminal "jelo")
>   ,ProductionRule Predicate $ Right (Terminal "jelo")
>   ,ProductionRule Content   $ Right (Terminal "jo")
>   ,ProductionRule Predicate $ Right (Terminal "jo")
>   ,ProductionRule Content   $ Right (Terminal "kala")
>   ,ProductionRule Predicate $ Right (Terminal "kala")
>   ,ProductionRule Content   $ Right (Terminal "kalama")
>   ,ProductionRule Predicate $ Right (Terminal "kalama")
>   ,ProductionRule Content   $ Right (Terminal "kama")
>   ,ProductionRule Predicate $ Right (Terminal "kama")
>   ,ProductionRule Content   $ Right (Terminal "kasi")
>   ,ProductionRule Predicate $ Right (Terminal "kasi")
>   ,ProductionRule Content   $ Right (Terminal "ken")
>   ,ProductionRule Predicate $ Right (Terminal "ken")
>   ,ProductionRule Content   $ Right (Terminal "kili")
>   ,ProductionRule Predicate $ Right (Terminal "kili")
>   ,ProductionRule Content   $ Right (Terminal "kiwen")
>   ,ProductionRule Predicate $ Right (Terminal "kiwen")
>   ,ProductionRule Content   $ Right (Terminal "ko")
>   ,ProductionRule Predicate $ Right (Terminal "ko")
>   ,ProductionRule Content   $ Right (Terminal "kon")
>   ,ProductionRule Predicate $ Right (Terminal "kon")
>   ,ProductionRule Content   $ Right (Terminal "kule")
>   ,ProductionRule Predicate $ Right (Terminal "kule")
>   ,ProductionRule Content   $ Right (Terminal "kulupu")
>   ,ProductionRule Predicate $ Right (Terminal "kulupu")
>   ,ProductionRule Content   $ Right (Terminal "kute")
>   ,ProductionRule Predicate $ Right (Terminal "kute")
>   ,ProductionRule Content   $ Right (Terminal "lape")
>   ,ProductionRule Predicate $ Right (Terminal "lape")
>   ,ProductionRule Content   $ Right (Terminal "laso")
>   ,ProductionRule Predicate $ Right (Terminal "laso")
>   ,ProductionRule Content   $ Right (Terminal "lawa")
>   ,ProductionRule Predicate $ Right (Terminal "lawa")
>   ,ProductionRule Content   $ Right (Terminal "len")
>   ,ProductionRule Predicate $ Right (Terminal "len")
>   ,ProductionRule Content   $ Right (Terminal "lete")
>   ,ProductionRule Predicate $ Right (Terminal "lete")
>   ,ProductionRule Content   $ Right (Terminal "lili")
>   ,ProductionRule Predicate $ Right (Terminal "lili")
>   ,ProductionRule Content   $ Right (Terminal "linja")
>   ,ProductionRule Predicate $ Right (Terminal "linja")
>   ,ProductionRule Content   $ Right (Terminal "lipu")
>   ,ProductionRule Predicate $ Right (Terminal "lipu")
>   ,ProductionRule Content   $ Right (Terminal "loje")
>   ,ProductionRule Predicate $ Right (Terminal "loje")
>   ,ProductionRule Content   $ Right (Terminal "luka")
>   ,ProductionRule Predicate $ Right (Terminal "luka")
>   ,ProductionRule Content   $ Right (Terminal "oko")
>   ,ProductionRule Predicate $ Right (Terminal "oko")
>   ,ProductionRule Content   $ Right (Terminal "lukin")
>   ,ProductionRule Predicate $ Right (Terminal "lukin")
>   ,ProductionRule Content   $ Right (Terminal "lupa")
>   ,ProductionRule Predicate $ Right (Terminal "lupa")
>   ,ProductionRule Content   $ Right (Terminal "ma")
>   ,ProductionRule Predicate $ Right (Terminal "ma")
>   ,ProductionRule Content   $ Right (Terminal "mama")
>   ,ProductionRule Predicate $ Right (Terminal "mama")
>   ,ProductionRule Content   $ Right (Terminal "mani")
>   ,ProductionRule Predicate $ Right (Terminal "mani")
>   ,ProductionRule Content   $ Right (Terminal "meli")
>   ,ProductionRule Predicate $ Right (Terminal "meli")
>   ,ProductionRule Content   $ Right (Terminal "mi")
>   ,ProductionRule Predicate $ Right (Terminal "mi")
>   ,ProductionRule Content   $ Right (Terminal "mije")
>   ,ProductionRule Predicate $ Right (Terminal "mije")
>   ,ProductionRule Content   $ Right (Terminal "moku")
>   ,ProductionRule Predicate $ Right (Terminal "moku")
>   ,ProductionRule Content   $ Right (Terminal "moli")
>   ,ProductionRule Predicate $ Right (Terminal "moli")
>   ,ProductionRule Content   $ Right (Terminal "monsi")
>   ,ProductionRule Predicate $ Right (Terminal "monsi")
>   ,ProductionRule Content   $ Right (Terminal "mun")
>   ,ProductionRule Predicate $ Right (Terminal "mun")
>   ,ProductionRule Content   $ Right (Terminal "musi")
>   ,ProductionRule Predicate $ Right (Terminal "musi")
>   ,ProductionRule Content   $ Right (Terminal "mute")
>   ,ProductionRule Predicate $ Right (Terminal "mute")
>   ,ProductionRule Content   $ Right (Terminal "nanpa")
>   ,ProductionRule Predicate $ Right (Terminal "nanpa")
>   ,ProductionRule Content   $ Right (Terminal "nasa")
>   ,ProductionRule Predicate $ Right (Terminal "nasa")
>   ,ProductionRule Content   $ Right (Terminal "nasin")
>   ,ProductionRule Predicate $ Right (Terminal "nasin")
>   ,ProductionRule Content   $ Right (Terminal "nena")
>   ,ProductionRule Predicate $ Right (Terminal "nena")
>   ,ProductionRule Content   $ Right (Terminal "ni")
>   ,ProductionRule Predicate $ Right (Terminal "ni")
>   ,ProductionRule Content   $ Right (Terminal "nimi")
>   ,ProductionRule Predicate $ Right (Terminal "nimi")
>   ,ProductionRule Content   $ Right (Terminal "noka")
>   ,ProductionRule Predicate $ Right (Terminal "noka")
>   ,ProductionRule Content   $ Right (Terminal "olin")
>   ,ProductionRule Predicate $ Right (Terminal "olin")
>   ,ProductionRule Content   $ Right (Terminal "ona")
>   ,ProductionRule Predicate $ Right (Terminal "ona")
>   ,ProductionRule Content   $ Right (Terminal "open")
>   ,ProductionRule Predicate $ Right (Terminal "open")
>   ,ProductionRule Content   $ Right (Terminal "pakala")
>   ,ProductionRule Predicate $ Right (Terminal "pakala")
>   ,ProductionRule Content   $ Right (Terminal "pali")
>   ,ProductionRule Predicate $ Right (Terminal "pali")
>   ,ProductionRule Content   $ Right (Terminal "palisa")
>   ,ProductionRule Predicate $ Right (Terminal "palisa")
>   ,ProductionRule Content   $ Right (Terminal "pan")
>   ,ProductionRule Predicate $ Right (Terminal "pan")
>   ,ProductionRule Content   $ Right (Terminal "pana")
>   ,ProductionRule Predicate $ Right (Terminal "pana")
>   ,ProductionRule Content   $ Right (Terminal "pilin")
>   ,ProductionRule Predicate $ Right (Terminal "pilin")
>   ,ProductionRule Content   $ Right (Terminal "pimeja")
>   ,ProductionRule Predicate $ Right (Terminal "pimeja")
>   ,ProductionRule Content   $ Right (Terminal "pini")
>   ,ProductionRule Predicate $ Right (Terminal "pini")
>   ,ProductionRule Content   $ Right (Terminal "pipi")
>   ,ProductionRule Predicate $ Right (Terminal "pipi")
>   ,ProductionRule Content   $ Right (Terminal "poka")
>   ,ProductionRule Predicate $ Right (Terminal "poka")
>   ,ProductionRule Content   $ Right (Terminal "poki")
>   ,ProductionRule Predicate $ Right (Terminal "poki")
>   ,ProductionRule Content   $ Right (Terminal "pona")
>   ,ProductionRule Predicate $ Right (Terminal "pona")
>   ,ProductionRule Content   $ Right (Terminal "pu")
>   ,ProductionRule Predicate $ Right (Terminal "pu")
>   ,ProductionRule Content   $ Right (Terminal "sama")
>   ,ProductionRule Predicate $ Right (Terminal "sama")
>   ,ProductionRule Content   $ Right (Terminal "seli")
>   ,ProductionRule Predicate $ Right (Terminal "seli")
>   ,ProductionRule Content   $ Right (Terminal "selo")
>   ,ProductionRule Predicate $ Right (Terminal "selo")
>   ,ProductionRule Content   $ Right (Terminal "sewi")
>   ,ProductionRule Predicate $ Right (Terminal "sewi")
>   ,ProductionRule Content   $ Right (Terminal "sijelo")
>   ,ProductionRule Predicate $ Right (Terminal "sijelo")
>   ,ProductionRule Content   $ Right (Terminal "sike")
>   ,ProductionRule Predicate $ Right (Terminal "sike")
>   ,ProductionRule Content   $ Right (Terminal "sin")
>   ,ProductionRule Predicate $ Right (Terminal "sin")
>   ,ProductionRule Content   $ Right (Terminal "namako")
>   ,ProductionRule Predicate $ Right (Terminal "namako")
>   ,ProductionRule Content   $ Right (Terminal "sina")
>   ,ProductionRule Predicate $ Right (Terminal "sina")
>   ,ProductionRule Content   $ Right (Terminal "sinpin")
>   ,ProductionRule Predicate $ Right (Terminal "sinpin")
>   ,ProductionRule Content   $ Right (Terminal "sitelen")
>   ,ProductionRule Predicate $ Right (Terminal "sitelen")
>   ,ProductionRule Content   $ Right (Terminal "sona")
>   ,ProductionRule Predicate $ Right (Terminal "sona")
>   ,ProductionRule Content   $ Right (Terminal "soweli")
>   ,ProductionRule Predicate $ Right (Terminal "soweli")
>   ,ProductionRule Content   $ Right (Terminal "suli")
>   ,ProductionRule Predicate $ Right (Terminal "suli")
>   ,ProductionRule Content   $ Right (Terminal "suno")
>   ,ProductionRule Predicate $ Right (Terminal "suno")
>   ,ProductionRule Content   $ Right (Terminal "supa")
>   ,ProductionRule Predicate $ Right (Terminal "supa")
>   ,ProductionRule Content   $ Right (Terminal "suwi")
>   ,ProductionRule Predicate $ Right (Terminal "suwi")
>   ,ProductionRule Content   $ Right (Terminal "taso")
>   ,ProductionRule Predicate $ Right (Terminal "taso")
>   ,ProductionRule Content   $ Right (Terminal "tawa")
>   ,ProductionRule Predicate $ Right (Terminal "tawa")
>   ,ProductionRule Content   $ Right (Terminal "telo")
>   ,ProductionRule Predicate $ Right (Terminal "telo")
>   ,ProductionRule Content   $ Right (Terminal "tenpo")
>   ,ProductionRule Predicate $ Right (Terminal "tenpo")
>   ,ProductionRule Content   $ Right (Terminal "toki")
>   ,ProductionRule Predicate $ Right (Terminal "toki")
>   ,ProductionRule Content   $ Right (Terminal "tomo")
>   ,ProductionRule Predicate $ Right (Terminal "tomo")
>   ,ProductionRule Content   $ Right (Terminal "tu")
>   ,ProductionRule Predicate $ Right (Terminal "tu")
>   ,ProductionRule Content   $ Right (Terminal "unpa")
>   ,ProductionRule Predicate $ Right (Terminal "unpa")
>   ,ProductionRule Content   $ Right (Terminal "uta")
>   ,ProductionRule Predicate $ Right (Terminal "uta")
>   ,ProductionRule Content   $ Right (Terminal "utala")
>   ,ProductionRule Predicate $ Right (Terminal "utala")
>   ,ProductionRule Content   $ Right (Terminal "walo")
>   ,ProductionRule Predicate $ Right (Terminal "walo")
>   ,ProductionRule Content   $ Right (Terminal "wan")
>   ,ProductionRule Predicate $ Right (Terminal "wan")
>   ,ProductionRule Content   $ Right (Terminal "waso")
>   ,ProductionRule Predicate $ Right (Terminal "waso")
>   ,ProductionRule Content   $ Right (Terminal "wawa")
>   ,ProductionRule Predicate $ Right (Terminal "wawa")
>   ,ProductionRule Content   $ Right (Terminal "weka")
>   ,ProductionRule Predicate $ Right (Terminal "weka")
>   ,ProductionRule Content   $ Right (Terminal "wile")
>   ,ProductionRule Predicate $ Right (Terminal "wile")
>   ]

> type Grammar = [ProductionRule]


Given a symbol or a pair of symbols (this should eventually become a *list* of
symbols in for non-CNF grammars), determine whether the given production rule
can produce the symbol(s).

> checkRule :: Either (Symbol,Symbol) Symbol -> ProductionRule -> Maybe Symbol
> checkRule s (ProductionRule n t)  = if s == t then Just n else Nothing


Given a grammar, send a symbol to all symbols which connect to it via a
production rule. This is relevant for the terminal production rules.

> zerothparse :: Grammar -> Symbol -> [Symbol]
> zerothparse g s = mapMaybe (checkRule $ Right s) g

> firstparse :: Grammar -> [Symbol] -> [[Symbol]] -- List of lists
> firstparse g = map $ zerothparse g


Given a grammar and list of symbols produced from the lexer, produce the
triangular array of the CYK algorithm.

> secondparse :: Grammar -> (Symbol,Symbol) -> [Symbol]
> secondparse g s = mapMaybe (checkRule $ Left s) g

> getPairs :: [a] -> [a] -> [(a,a)]
> getPairs u v = [(x,y) | x<-u, y<-v]

> nextLevel :: Grammar -> [[[Symbol]]] -> [[Symbol]]
> nextLevel g [] = [] -- FIXME
> nextLevel g t = l : nextLevel g (map tail t)
>   where
>     l = concat . concat $
>       map (\i->(map $ secondparse g) $
>         getPairs (t !! i !! 0) (t !! (n-i-1) !! (1+i))) [0..n-1]
>     n = length (t !! 0) -length t


This parser compiles correctly, but does not work properly.

> parse :: Grammar -> [Symbol] -> [[[Symbol]]] -- "Array" of lists
> parse g ss = p:ps
>   where
>     p  = firstparse g ss
>     ps = map (nextLevel g . (p:)) $ inits ps

Playground:

> s = "mi moku"
> p = lexer s
> -- n = firstparse grammar p
> n = [[SubjectPhrase],[Predicate]]
> n' = nextLevel grammar [n]
>
> x = zipWith ((,)) [1,2] [3,4]
> y = [(x,y) | x<-[1,2], y<-[3,4]]

< a :: Main.Lexeme
< a = Seperator ","

< s = "mi wile e ni: mi kama sona e jan pi lon ni."
< p = lexer s

< Nonterminal "start" [(Content "mi", Predicate)
<                   ,(Content "sina", Predicate)
<                   ,(SubjectPhrase, Predicate)
<                   ]
< Nonterminal "subjectPhrase" [(Content, Seperator "li")]

< Content -> Content
<               | [Content, Content]
<               | [Content, Termial "pi", Content]

Playtesting a tree

> t :: Tree Int Bool
> t = Node 3 (Node 5 (Leaf True) (Leaf False)) (Leaf False)

