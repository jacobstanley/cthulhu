module Pretty (render) where

import           Data.List (intersperse)
import           Text.PrettyPrint hiding (render)
import qualified Text.PrettyPrint as P

import Types hiding (char, brackets, parens, (<>))

------------------------------------------------------------
-- Regex Pretty Printing

instance Pretty (RegexM a) where
    pretty Empty        = empty
    pretty Start        = char '^'
    pretty End          = char '$'
    pretty (Expr x)     = pretty x
    pretty (Many x)     = pretty x <> char '*'
    pretty (Many1 x)    = pretty x <> char '+'
    pretty (Fewest x)   = pretty x <> text "*?"
    pretty (Fewest1 x)  = pretty x <> text "+?"
    pretty (Optional x) = pretty x <> char '?'
    pretty (Join x y)   = pretty x <> pretty y
    pretty (Choice x y) = parens (text "?:" <> pretty x <> char '|' <> pretty y)

instance Pretty Expr where
    pretty Any              = char '.'
    pretty (Char c)         = escape c
    pretty (OneOf xs)       = brackets (hcat' xs)
    pretty (NoneOf xs)      = brackets (char '^' <> hcat' xs)
    pretty (Capture r)      = parens (pretty r)
    pretty (Named name r)   = parens (text "?P<" <> text name <> char '>' <> pretty r)
    pretty (SubExpr r)      = parens (text "?:" <> pretty r)
    pretty (LookAhead r)    = parens (text "?=" <> pretty r)
    pretty (NegLookAhead r) = parens (text "?!" <> pretty r)
    pretty (Backref n)      = char '\\' <> number n

instance Pretty Item where
    pretty (Item c)    = escape c
    pretty (Range x y) = escape x <> char '-' <> escape y

escape :: Char -> Doc
escape '\t'          = text "\\t"
escape '\r'          = text "\\r"
escape '\n'          = text "\\n"
escape '\f'          = text "\\f"
escape '\xa0'        = text "\\xa0"
escape c | meta c    = char '\\' <> char c
         | otherwise = char c
  where
    meta = (`elem` "^$.*+?|[()\\")

number :: Int -> Doc
number = text . show

class Pretty a where
    pretty :: a -> Doc

render :: Pretty a => a -> String
render = P.render . pretty

hcat' :: Pretty a => [a] -> Doc
hcat' = hcat . map pretty
