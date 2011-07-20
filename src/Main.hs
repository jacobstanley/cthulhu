{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (not, id)

import Data.Monoid
import Test.QuickCheck hiding (oneof)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE hiding (Regex)

import Types
import Pretty

test = stackoverflow


stackoverflow = angles (name +> many attr)
  where
    name = lexeme $ letter +> many alphaNum
    attr = lexeme $ name +> optional (char '=' +> (doubleQuoted <|> singleQuoted))



html = openTag <|> closeTag <|> content

openTag = angles
     $  tagName "open"
     +> many attribute
     +> optional (char '/')

closeTag = angles $ char '/' +> tagName "close"

attribute = lexeme
    $  tagName "attr"
    +> char '='
    +> captureSingles <|> captureDoubles

content = named "content" (many $ noneOf "<>")

tagName n = lexeme $ named n (letter +> many alphaNum)

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

    --body = "<html style='no'><head><h1 class='fred' onclick=\"ok\">x</h1></head></html>"
    body = "<p> <a href=\"foo\"> <br /> <hr class=\"foo\"/> <div style=\"background:url('/path/image/.jpg');\" title='Yes/No'>  <input value=\"is 5 > 3?\" />  <input disabled></input>"
    pattern = render test

{-
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
-}
