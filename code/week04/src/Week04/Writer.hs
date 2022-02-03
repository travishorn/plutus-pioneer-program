module Week04.Writer where

import Control.Monad
import Week04.Monad

-- A new type that takes two arguments. a is the result and [String] is a list
-- of log messages
data Writer a = Writer a [String]
    deriving Show

-- Function that takes an Int and returns Writer Int
number :: Int -> Writer Int
-- Whatever number n is passed in gets passed to Writer as the first argument.
-- The second argument to Writer is a list with a single string inside that acts
-- as a log of sorts
number n = Writer n $ ["number: " ++ show n]

-- number 42
-- Writer 42 ["number: 42"]

-- bar takes three Writers with single Int each. Returns a Writer with the sum
-- and log messages about the input numbers
bar :: Writer Int -> Writer Int -> Writer Int -> Writer Int
bar (Writer k xs) (Writer l ys) (Writer m zs) =
  -- Return Writer with sum as first arg and log messages as second arg
  Writer (k + l + m) $ xs ++ ys ++ zs

-- bar (number 1) (number 2) (number 3)
-- Writer 6 ["number: 1","number: 2","number: 3"]


-- Helper function that takes a list of strings and returns a Writer
-- We use () because we don't care about the result in this Writer, just the log
-- mesages
tell :: [String] -> Writer ()
-- Idiomatic version
-- tell xs = Writer () xs

-- Point-free version. Result is identical to idiomatic version above
tell = Writer ()

-- Like bar above, foo takes three Writers with single Int each. Returns a
-- Writer with the sum and log messages about the input numbers. This time, it
-- also includes the sum in the log messages.
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    -- sum the three Ints and store as s
    s = k + l + m
    -- A partial writer with only a log part. We drop the value. The log part
    -- is the sum calculated above as s
    Writer _ us = tell ["sum: " ++ show s]
  in
    -- Return a Writer with the sum and log messages about the input numbers and
    -- the sum
    Writer s $ xs ++ ys ++ zs ++ us

-- foo (number 1) (number 2) (number 3)
-- Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]

-- Like bindMaybe in Maybe.hs and bindEither in Either.hs, we can make a
-- bindWriter
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys

-- Rewrite foo to use bindWriter. We don't have to do pattern matching to
-- extract log messages and we don't have to combined the log messages. All is
-- abstracted away in bindWriter
foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
             y `bindWriter` \l ->
             z `bindWriter` \m ->
             let s = k + l + m
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                Writer s []

-- foo' (number 1) (number 2) (number 3)
-- Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]


-- Implementing a Monad manually

-- Standard way to implement Functor and Applicative is to implement helper
-- functions from `Control.Monad` which are imported at the top of this file.

instance Functor Writer where
    -- liftM from Control.Monad makes use of return and bind to implement fmap
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter


-- Use the threeInts function defined in Week04.Monad and imported at the top of
-- this file. This example uses `do` notation instead of explicit binds
foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = do
    s <- threeInts x y z
    tell ["sum: " ++ show s]
    return s
