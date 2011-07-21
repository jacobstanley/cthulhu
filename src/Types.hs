{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.Reify
import Control.Applicative hiding ((<|>), many)

------------------------------------------------------------
-- Regex Types

data Regex
    = Start
    | End
    | Expr Expr
    | Many Expr
    | Many1 Expr
    | Fewest Expr
    | Fewest1 Expr
    | Optional Expr
    | Append Regex Regex
    | Choice Regex Regex
    deriving (Show)

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
    deriving (Show)

data Item = Item Char | Range Char Char
    deriving (Show)

------------------------------------------------------------
-- Regex - Observable Sharing

data RegexGraph = RegexGraph [(Unique, RegexNode Unique)] Unique

data RegexNode s
    = StartN
    | EndN
    | ExprN s
    | ManyN s
    | Many1N s
    | FewestN s
    | Fewest1N s
    | OptionalN s
    | AppendN s s
    | ChoiceN s s
    ------------------------
    | AnyN
    | CharN Char
    | OneOfN [Item]
    | NoneOfN [Item]
    | SubExprN s
    | CaptureN s
    | NamedN String s
    | LookAheadN s
    | NegLookAheadN s
    | BackrefN Int
    deriving (Show)

instance MuRef Regex where
    type DeRef Regex = RegexNode
    mapDeRef f Start        = pure StartN
    mapDeRef f End          = pure EndN
    mapDeRef f (Expr x)     = ExprN <$> f x
    mapDeRef f (Many x)     = ManyN <$> f x
    mapDeRef f (Many1 x)    = Many1N <$> f x
    mapDeRef f (Fewest x)   = FewestN <$> f x
    mapDeRef f (Fewest1 x)  = Fewest1N <$> f x
    mapDeRef f (Optional x) = OptionalN <$> f x
    mapDeRef f (Append x y) = AppendN <$> f x <*> f y
    mapDeRef f (Choice x y) = ChoiceN <$> f x <*> f y

instance MuRef Expr where
    type DeRef Expr = RegexNode
    mapDeRef f Any              = pure AnyN
    mapDeRef f (Char x)         = pure $ CharN x
    mapDeRef f (OneOf xs)       = pure $ OneOfN xs
    mapDeRef f (NoneOf xs)      = pure $ NoneOfN xs
    mapDeRef f (SubExpr x)      = SubExprN <$> f x
    mapDeRef f (Capture x)      = CaptureN <$> f x
    mapDeRef f (Named n x)      = NamedN n <$> f x
    mapDeRef f (LookAhead x)    = LookAheadN <$> f x
    mapDeRef f (NegLookAhead x) = NegLookAheadN <$> f x
    mapDeRef f (Backref x)      = pure $ BackrefN x

------------------------------------------------------------
-- Regex DSL - Primitives

regex :: Expr -> Regex
regex = Expr

expr :: Regex -> Expr
expr = SubExpr

infixr 0 +>
infixr 1 <|>

(+>) :: Regex -> Regex -> Regex
(+>) = Append

join :: [Regex] -> Regex
join [] = error "join: cannot do join on empty list"
join xs = foldl1 (+>) xs

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
between open close x = open +> x +> close

angles :: Regex -> Regex
angles = between (char '<') (char '>')

parens :: Regex -> Regex
parens = between (char '(') (char ')')

braces :: Regex -> Regex
braces = between (char '{') (char '}')

brackets :: Regex -> Regex
brackets = between (char '[') (char ']')

lexeme :: Regex -> Regex
lexeme = (+> spaces)

manyTill :: Regex -> Regex -> Regex
manyTill r end = fewest r +> followedBy end

singleQuoted :: Regex
singleQuoted = char '\'' +> many (noneOf "\'") +> char '\''

doubleQuoted :: Regex
doubleQuoted = char '\"' +> many (noneOf "\"") +> char '\"'

captureSingles :: Regex
captureSingles = char '\'' +> capture (many $ noneOf "\'") +> char '\''

captureDoubles :: Regex
captureDoubles = char '\"' +> capture (many $ noneOf "\"") +> char '\"'
