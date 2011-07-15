{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (not)

import Data.Monoid
import Test.QuickCheck hiding (oneof)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE hiding (Regex)

--import Types
--import Pretty
--
--test = mconcat
--    [ char '<'
--    , capture $ many1 $ noneof " />"
--    , many0 whitespace
--    , optional $ capture $ many1 $ noneof "="
--    ]

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
    pattern = "(html)"
    --pattern = renderP test

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
