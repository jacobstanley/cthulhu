{-# LANGUAGE TemplateHaskell #-}

module Language.CSharp.Arbitrary () where

import Control.Applicative
import Data.DeriveTH
import Data.List (intersperse, nub)
import Text.PrettyPrint
import Test.QuickCheck

------------------------------------------------------------
-- Regex Types

data Regex = Regex [[BasicExpr]]
    deriving (Show, Eq)

data BasicExpr = Expr Expr | Many Expr | Many1 Expr
    deriving (Show, Eq)

data Expr
    = Any
    | Start
    | End
    | Char Char
    | Set [Item]
    | NSet [Item]
    | Group Regex
    deriving (Show, Eq)

data Item = Item Char | Range Char Char
    deriving (Show, Eq)

------------------------------------------------------------
-- Regex Arbitrary

instance Arbitrary Regex where
    arbitrary = Regex <$> listOf1 (listOf1 arbitrary)

instance Arbitrary BasicExpr where
    arbitrary = oneof
        [ Expr  <$> arbitrary
        , Many  <$> arbitrary
        , Many1 <$> arbitrary ]

instance Arbitrary Expr where
    arbitrary = oneof
        [ pure Any
        , pure Start
        , pure End
        , Char  <$> ascii
        , Set   <$> uniques1 arbitrary
        , NSet  <$> uniques1 arbitrary
        , Group <$> arbitrary ]

instance Arbitrary Item where
    arbitrary = oneof
        [ Item  <$> ascii
        , Range <$> ascii <*> ascii ]

ascii :: Gen Char
ascii = choose ('!', '~')

uniques1 :: Eq a => Gen a -> Gen [a]
uniques1 g = nub <$> listOf1 g

------------------------------------------------------------
-- Regex Pretty Printing

instance Pretty Regex where
    pretty (Regex xs) = hcat $ intersperse (char '|') $ map hcat' xs

instance Pretty BasicExpr where
    pretty (Expr x)  = pretty x
    pretty (Many x)  = pretty x <> char '*'
    pretty (Many1 x) = pretty x <> char '+'

instance Pretty Expr where
    pretty Any       = char '.'
    pretty Start     = char '^'
    pretty End       = char '$'
    pretty (Char c)  = char c
    pretty (Set xs)  = brackets (hcat' xs)
    pretty (NSet xs) = brackets (char '^' <> hcat' xs)
    pretty (Group g) = parens (pretty g)

instance Pretty Item where
    pretty (Item c)    = escape c
    pretty (Range x y) = escape x <> char '-' <> escape y

escape :: Char -> Doc
escape c | meta c    = char '\\' <> char c
         | otherwise = char c
  where
    meta = (`elem` "\\|*+.[$()")

class Pretty a where
    pretty :: a -> Doc

renderP :: Pretty a => a -> String
renderP = render . pretty

hcat' :: Pretty a => [a] -> Doc
hcat' = hcat . map pretty

------------------------------------------------------------
-- Regex DSL

------------------------------------------------------------
-- Main

--putStrLn (renderP $ NegativeSet [Item '*', Range 'a' 'z'])
--putStrLn (renderP $ Regex [[Expr Any],[Expr $ Set $ PositiveSet [Item 'A']]])

main :: IO ()
main = do
    xs <- sample' $ resize 3 (arbitrary :: Gen Regex)
    mapM_ (putStrLn . renderP) (take 3 $ xs)
