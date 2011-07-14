module Arbitrary where

import Control.Applicative
import Data.List (nub, sort)
import Test.QuickCheck

import Types

------------------------------------------------------------
-- Regex Arbitrary

instance Arbitrary Regex where
    arbitrary = Regex <$> listOf1 (listOf1 arbitrary)

instance Arbitrary BasicExpr where
    arbitrary = oneof
        [ pure Start
        , pure End
        , expr 0 10
        , Many0 <$> arbitrary
        , Many1 <$> arbitrary ]
      where
        expr :: Int -> Int -> Gen BasicExpr
        expr m n = do
            [m', n'] <- sort <$> vectorOf 2 (choose (m, n))
            Expr m' n' <$> arbitrary

instance Arbitrary Expr where
    arbitrary = oneof
        [ pure Any
        , Char     <$> ascii
        , Set      <$> uniques1 arbitrary
        , NSet     <$> uniques1 arbitrary
        , Capture  <$> arbitrary
        , Group    <$> arbitrary
        , NegAhead <$> arbitrary
        ]

instance Arbitrary Item where
    arbitrary = oneof
        [ Item  <$> suchThat ascii (/= '-')
        , range '0' '9'
        , range 'A' 'Z'
        , range 'a' 'z' ]
      where
        range :: Char -> Char -> Gen Item
        range x y = do
            [x', y'] <- sort <$> vectorOf 2 (choose (x, y))
            return $ Range x' y'

ascii :: Gen Char
ascii = choose ('!', '~')

uniques1 :: Eq a => Gen a -> Gen [a]
uniques1 g = nub <$> listOf1 g

