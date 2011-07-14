module Pretty (renderP) where

import Data.List (intersperse)
import Text.PrettyPrint

import Types

------------------------------------------------------------
-- Regex Pretty Printing

instance Pretty Regex where
    pretty (Regex xs) = hcat $ intersperse (char '|') $ map hcat' xs

instance Pretty BasicExpr where
    pretty Start        = char '^'
    pretty End          = char '$'
    pretty (Expr 1 1 x) = pretty x
    pretty (Expr 0 1 x) = pretty x <> char '?'
    pretty (Expr m n x) = pretty x <> braces (number m <> char ',' <> number n)
    pretty (Many0 x)     = pretty x <> char '*'
    pretty (Many1 x)     = pretty x <> char '+'

instance Pretty Expr where
    pretty Any          = char '.'
    pretty (Char c)     = escape c
    pretty (Set xs)     = brackets (hcat' xs)
    pretty (NSet xs)    = brackets (char '^' <> hcat' xs)
    pretty (Capture r)  = parens (pretty r)
    pretty (Group r)    = parens (text "?:" <> pretty r)
    pretty (NegAhead r) = parens (text "?!" <> pretty r)

instance Pretty Item where
    pretty (Item c)    = escape c
    pretty (Range x y) = escape x <> char '-' <> escape y

escape :: Char -> Doc
escape '\t'          = char '\\' <> char 't'
escape '\r'          = char '\\' <> char 'r'
escape '\n'          = char '\\' <> char 'n'
escape c | meta c    = char '\\' <> char c
         | otherwise = char c
  where
    meta = (`elem` "^$.*+?|[()\\")

number :: Int -> Doc
number = text . show

class Pretty a where
    pretty :: a -> Doc

renderP :: Pretty a => a -> String
renderP = render . pretty

hcat' :: Pretty a => [a] -> Doc
hcat' = hcat . map pretty

