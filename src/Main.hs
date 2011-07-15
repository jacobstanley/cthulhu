{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (not)

import Data.Monoid
import Test.QuickCheck hiding (oneof)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE hiding (Regex)

import Types
import Pretty

------------------------------------------------------------
-- Regex DSL

instance Monoid Regex where
    mempty = Regex []

    mappend x (Regex []) = x
    mappend (Regex []) x = x
    mappend (Regex [x]) (Regex [y]) = Regex [x++y]
    mappend x y = group x `mappend` group y

group :: Regex -> Regex
group = expr . Group

capture :: Regex -> Regex
capture = expr . Capture

char :: Char -> Regex
char = expr . Char

oneof :: [Char] -> Regex
oneof = expr . Set . map Item

noneof :: [Char] -> Regex
noneof = expr . NSet . map Item

not :: Regex -> Regex
-- Char -> NSet
not (Regex [[Many0 (Char c)]])    = Regex [[Many0 $ NSet [Item c]]]
not (Regex [[Many1 (Char c)]])    = Regex [[Many1 $ NSet [Item c]]]
not (Regex [[Expr m n (Char c)]]) = Regex [[Expr m n $ NSet [Item c]]]
-- Set -> NSet
not (Regex [[Many0 (Set xs)]])    = Regex [[Many0 $ NSet xs]]
not (Regex [[Many1 (Set xs)]])    = Regex [[Many1 $ NSet xs]]
not (Regex [[Expr m n (Set xs)]]) = Regex [[Expr m n $ NSet xs]]
-- NSet -> Set
not (Regex [[Many0 (NSet xs)]])    = Regex [[Many0 $ Set xs]]
not (Regex [[Many1 (NSet xs)]])    = Regex [[Many1 $ Set xs]]
not (Regex [[Expr m n (NSet xs)]]) = Regex [[Expr m n $ Set xs]]
-- everything else
not x = expr (NegAhead x)

expr :: Expr -> Regex
expr x = Regex [[Expr 1 1 x]]

many0 :: Regex -> Regex
many0 (Regex [[Expr 1 1 x]]) = Regex [[Many0 x]]
many0 x                      = many1 (group x)

many1 :: Regex -> Regex
many1 (Regex [[Expr 1 1 x]]) = Regex [[Many1 x]]
many1 x                      = many1 (group x)

optional :: Regex -> Regex
optional (Regex [[Expr 1 1 x]]) = Regex [[Expr 0 1 x]]
optional x                      = optional (group x)

whitespace :: Regex
whitespace = oneof " \t\r\n"

mor x (Regex []) = x
mor (Regex []) x = x
mor (Regex [x]) (Regex [y]) = Regex [x, y]
mor x y = group x `mor` group y

test = mconcat
    [ char '<'
    , capture $ many1 $ noneof " />"
    , many0 whitespace
    , optional $ capture $ many1 $ noneof "="
    ]

------------------------------------------------------------
-- Main

main :: IO ()
main = do
    putStrLn "HTML Regex\n"
    putStrLn $ pattern ++ "\n" ++ body ++ "\n"

    mapM_ print matches
  where
    matches :: [[String]]
    matches = body =~ pattern

    body = "<html><head style='no'><h1 onclick=\"ok\">x</h1></head></html>"
    pattern = renderP test

    --putStrLn "Random Regex"
    --xs <- sample' $ resize 4 (arbitrary :: Gen Regex)
    --mapM_ (putStrLn . renderP) (take 3 $ xs)

    --putStrLn "\nQuickCheck"
    --result <- quickCheckWithResult args prop_check
    --putStrLn $ "replay = read \"Just ("
    --        ++ show (usedSeed result) ++ ", "
    --        ++ show (usedSize result) ++ ")\""
  --where
  --  args = stdArgs
  --       { maxSuccess = 1000000
  --       , maxSize = 4
  --       , chatty = False
  --       --, replay = read "Just (969080827 2147483396, 2)"
  --       }

prop_check (regex :: BasicExpr) =
    if text =~ pattern /= result
    then True
    else unsafePerformIO $ do
        putStrLn (text ++ " => " ++ result)
        putStrLn output
        return False
  where
    result  = "a3"
    text    = "abc12a3aaacc"
    pattern = renderP regex
    output  = pattern ++ "\n" ++ show regex
