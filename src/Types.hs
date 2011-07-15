module Types where

import Data.List (foldl')

------------------------------------------------------------
-- Regex Types

data Regex
    = Empty
    | Start
    | End
    | Expr Expr
    | Many Expr
    | Many1 Expr
    | Optional Expr
    | Join Regex Regex
    | Choice Regex Regex
    deriving (Show, Eq)

data Expr
    = Any
    | Char Char
    | OneOf [Item]
    | NoneOf [Item]
    | Capture Regex
    | NegAhead Regex
    | SubExpr Regex
    deriving (Show, Eq)

data Item = Item Char | Range Char Char
    deriving (Show, Eq)

------------------------------------------------------------
-- Regex DSL - Primitives

regex :: Expr -> Regex
regex = Expr

expr :: Regex -> Expr
expr = SubExpr

infixr 0 <+>
infixr 1 <|>

(<|>) :: Regex -> Regex -> Regex
(<|>) (Expr (OneOf xs)) (Expr (OneOf ys)) = Expr $ OneOf $ xs ++ ys
(<|>) x y = Choice x y

choice :: [Regex] -> Regex
choice = foldl' (<|>) Empty

(<+>) :: Regex -> Regex -> Regex
(<+>) x y = Join x y

join :: [Regex] -> Regex
join = foldl' (<+>) Empty

many :: Regex -> Regex
many (Expr x) = Many x
many x        = Many (expr x)

many1 :: Regex -> Regex
many1 (Expr x) = Many1 x
many1 x        = Many1 (expr x)

optional :: Regex -> Regex
optional (Expr x) = Optional x
optional x        = Optional (expr x)

anyChar :: Regex
anyChar = regex Any

char :: Char -> Regex
char = regex . Char

oneOf :: [Char] -> Regex
oneOf = regex . OneOf . map Item

range :: Char -> Char -> Regex
range x y = regex $ OneOf [Range x y]

noneOf :: [Char] -> Regex
noneOf = regex . NoneOf . map Item

capture :: Regex -> Regex
capture = regex . Capture

notFollowedBy :: Regex -> Regex -> Regex
notFollowedBy = regex . NegAhead

------------------------------------------------------------
-- Regex DSL - Characters

spaces :: Regex
spaces = many space

space :: Regex
space = oneOf " \t\r\n\f\v\xa0"

letter :: Regex
letter = range 'a' 'z' <|> range 'A' 'Z'

digit :: Regex
digit = range '0' '9'

hexDigit :: Regex
hexDigit = range '0' '9' <|> range 'a' 'f' <|> range 'A' 'F'

octDigit :: Regex
octDigit = range '0' '7'

string :: String -> Regex
string = join . map char

------------------------------------------------------------
-- Regex DSL - Combinators

between :: Regex -> Regex -> Regex -> Regex
between open close x = open <+> x <+> close

------------------------------------------------------------
-- Regex DSL

not :: Regex -> Regex
-- Char -> NoneOf
not (Many (Char c))  = Many $ NoneOf [Item c]
not (Many1 (Char c)) = Many1 $ NoneOf [Item c]
not (Expr (Char c))  = Expr $ NoneOf [Item c]
-- OneOf -> NoneOf
not (Many (OneOf xs))  = Many $ NoneOf xs
not (Many1 (OneOf xs)) = Many1 $ NoneOf xs
not (Expr (OneOf xs))  = Expr $ NoneOf xs
-- NoneOf -> OneOf
not (Many (NoneOf xs))  = Many $ OneOf xs
not (Many1 (NoneOf xs)) = Many1 $ OneOf xs
not (Expr (NoneOf xs))  = Expr $ OneOf xs
-- everything else
not x = regex (NegAhead x)
