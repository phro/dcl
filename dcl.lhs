~~ Lexer for Toki Pona ~~
Authors: Jesse Frohlich & Andrew Wilson
License: GPL3

> import Data.List.Split
> import Data.Char
> import Control.Monad

A rough classification of words in Toki Pona

> data Lexeme = Content String     -- Noun, verb, modifier
>             | Borrow String      -- (always modifiers)
>             | Preposition String -- e, tawa, kepeken, lon, li?, poka, &c.
>             | Seperator String   -- ",":".":"pi":[]
>             | Nonsense String    -- to collect errors
>             deriving (Show)


Lexer based on the lex function. This splits the string into words and
punctuation, each as their own string.

> lexer :: String -> [String]
> lexer [] = []
> lexer s  = n : lexer s'
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


Playground:

> a :: Main.Lexeme
> a = Seperator ","
>
> s = "mi wile e ni: mi kama sona e jan pi lon ni."
> p = lexer s
