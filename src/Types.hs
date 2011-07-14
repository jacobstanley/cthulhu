module Types where

------------------------------------------------------------
-- Regex Types

data Regex = Regex [[BasicExpr]]
    deriving (Show, Eq)

data BasicExpr
    = Start
    | End
    | Expr Int Int Expr
    | Many0 Expr
    | Many1 Expr
    deriving (Show, Eq)

data Expr
    = Any
    | Char Char
    | Set [Item]
    | NSet [Item]
    | Capture Regex
    | Group Regex
    | NegAhead Regex
    deriving (Show, Eq)

data Item = Item Char | Range Char Char
    deriving (Show, Eq)

