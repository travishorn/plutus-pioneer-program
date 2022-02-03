module Week04.Either where

import Text.Read (readMaybe)
import Week04.Monad

-- The problem with `Maybe` is that when there's an error, only `Nothing` is
-- returned. There's no place for an error message. We can use `Either` to solve
-- that problem.

-- readMaybe returns Maybe. We can wrap it in a new function that returns Either
-- instead.
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
    -- If readMaybe returns Nothing, return Left with an error string
    Nothing -> Left $ "can't parse: " ++ s
    -- If readMaybe returns a Just value, return Right with the value
    Just a  -> Right a

-- readEither "42" :: Either String Int
-- Right 42

-- readEither "x" :: Either String Int
-- Left "can't parse: x"


-- A rewrite of foo (from Maybe.hs) again, but using Either
foo :: String -> String -> String -> Either String Int
foo x y z = case readEither x of
    -- If readEither x is Left (error), return Left here too with the error
    Left err -> Left err
    -- If readEither x is Right (with a value), continue
    Right k  -> case readEither y of
        -- Repeat for readEither y
        Left err -> Left err
        -- Repeat for readEither z
        Right l  -> case readEither z of
            Left err -> Left err
            -- If we made it this far, all were parsed. Return the sum
            Right m  -> Right (k + l + m)

-- foo "1" "2" "3"
-- Right 6

-- foo "1" "2" "x"
-- Left "can't parse: x"


-- A rewrite of bindMaybe (from Maybe.hs) again, but using Either
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x

-- A rewrite of foo' (from Maybe.hs) again, but using Either and the newly
-- rewritten readEither and bindEither
foo' :: String -> String -> String -> Either String Int
foo' x y z = readEither x `bindEither` \k ->
             readEither y `bindEither` \l ->
             readEither z `bindEither` \m ->
             Right (k + l + m)

-- Use the threeInts function defined in Week04.Monad and imported at the top of
-- this file
foo'' :: String -> String -> String -> Either String Int
foo'' x y z = threeInts (readEither x) (readEither y) (readEither z)
