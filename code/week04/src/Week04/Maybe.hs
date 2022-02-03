module Week04.Maybe where

import Text.Read (readMaybe)
import Week04.Monad

-- The function should try to parse all three input strings as Ints. If that is
-- possible, add all three together and return the sum. If any of the parses
-- fails, return Nothing
foo :: String -> String -> String -> Maybe Int
foo x y z = case readMaybe x of
    -- If x can't be read, return Nothing
    Nothing -> Nothing
    -- If it can, see if y can be read
    Just k  -> case readMaybe y of
        -- If not, return Nothing
        Nothing -> Nothing
        -- If it can, see if z can be read
        Just l  -> case readMaybe z of
            -- If not, return Nothing
            Nothing -> Nothing
            -- If it can, return the sum
            Just m  -> Just (k + l + m)

-- foo "1" "2" "3"
-- Just 6

-- foo "1" "2" "x"
-- Nothing


-- The code above is repeated three times and its almost always the same. We can
-- refactor it like so:

-- Given Maybe a
-- and a function from a to Maybe b
-- Return a Maybe b
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- If Maybe a is nothing, we don't care about the function. Return Nothing
bindMaybe Nothing  _ = Nothing
-- If Maybe a is Just x, return the function with x passed in
bindMaybe (Just x) f = f x

-- foo again, but this time its using the power of the helpfer function we wrote
-- above
foo' :: String -> String -> String -> Maybe Int
             -- Try to read x. If successful, store it as k
foo' x y z = readMaybe x `bindMaybe` \k ->
             -- Try to read y. If successful, store it as l
             readMaybe y `bindMaybe` \l ->
             -- Try to read z. If successful, store it as m
             readMaybe z `bindMaybe` \m ->
             -- Return the sum. If any of the above reads fail, the whole
             -- function immediately returns Nothing
             Just (k + l + m)

-- Use the threeInts function defined in Week04.Monad and imported at the top of
-- this file
foo'' :: String -> String -> String -> Maybe Int
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
