{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.Monoid hiding (Any)
import Data.List (foldl')

------------------------------------------------------------
-- Regex Types

type Regex = RegexM ()

data RegexM a
    = Empty
    | Start
    | End
    | Expr Expr
    | Many Expr
    | Many1 Expr
    | Fewest Expr
    | Fewest1 Expr
    | Optional Expr
    | forall b c. Join (RegexM b) (RegexM c)
    | forall b c. Choice (RegexM b) (RegexM c)

data Expr
    = Any
    | Char Char
    | OneOf [Item]
    | NoneOf [Item]
    | SubExpr Regex
    | Capture Regex
    | Named String Regex
    | LookAhead Regex
    | NegLookAhead Regex
    | Backref Int

data Item = Item Char | Range Char Char

deriving instance Show (RegexM a)
deriving instance Show Expr
deriving instance Show Item

------------------------------------------------------------
-- Regex DSL - Type Classes

instance Monoid Regex where
    mempty  = Empty
    mappend = (<>)
    mconcat = join

instance Monad RegexM where
    return _ = Empty
    (>>)     = (<>)
    ma >>= f = ma >> f (error "Regex.Types.(>>=): _|_")

------------------------------------------------------------
-- Regex DSL - Primitives

regex :: Expr -> Regex
regex = Expr

expr :: Regex -> Expr
expr = SubExpr

infixr 0 <>
infixr 1 <|>

(<>) :: RegexM a -> RegexM b -> RegexM c
(<>) = Join

join :: [Regex] -> Regex
join = foldl' (<>) Empty

(<|>) :: Regex -> Regex -> Regex
(<|>) (Expr (OneOf xs)) (Expr (OneOf ys)) = Expr $ OneOf $ xs ++ ys
(<|>) x y = Choice x y

choice :: [Regex] -> Regex
choice [] = error "choice: cannot do choice on empty list"
choice xs = foldl1 (<|>) xs

many :: Regex -> Regex
many (Expr x) = Many x
many x        = Many (expr x)

many1 :: Regex -> Regex
many1 (Expr x) = Many1 x
many1 x        = Many1 (expr x)

fewest :: Regex -> Regex
fewest (Expr x) = Fewest x
fewest x        = Fewest (expr x)

fewest1 :: Regex -> Regex
fewest1 (Expr x) = Fewest1 x
fewest1 x        = Fewest1 (expr x)

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

named :: String -> Regex -> Regex
named name r = regex (Named name r)

followedBy :: Regex -> Regex
followedBy = regex . LookAhead

notFollowedBy :: Regex -> Regex
notFollowedBy = regex . NegLookAhead

backref :: Int -> Regex
backref = regex . Backref

------------------------------------------------------------
-- Regex DSL - Characters

spaces :: Regex
spaces = many space

space :: Regex
space = oneOf " \t\r\n\f\xa0"

alphaNum :: Regex
alphaNum = letter <|> digit

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
between open close x = open <> x <> close

angles :: Regex -> Regex
angles = between (char '<') (char '>')

parens :: Regex -> Regex
parens = between (char '(') (char ')')

braces :: Regex -> Regex
braces = between (char '{') (char '}')

brackets :: Regex -> Regex
brackets = between (char '[') (char ']')

lexeme :: Regex -> Regex
lexeme = (<> spaces)

manyTill :: Regex -> Regex -> Regex
manyTill r end = fewest r <> followedBy end

captureBetween :: Regex -> Regex -> Regex
captureBetween open close = open <> capture (manyTill anyChar close) <> close

captureQuoted :: Regex -> Regex
captureQuoted quote = captureBetween quote quote

captureSingles :: Regex
captureSingles = captureQuoted (char '\'')

captureDoubles :: Regex
captureDoubles = captureQuoted (char '\"')
