module Notes where

-- Bind example

-- You can perform IO actions in sequence with `>>`. If the first action has a
-- return result, it will be ignored.

-- If you need to use the result of the first action, you can use bind `>>=`.

-- It uses any monad, but we are interested in `IO`.
-- Given the first argument that has side-effects `a`: `IO a`
-- And given the second argument that is a function takes an `a` and returns
-- `IO b`: `(a -> m b)`
-- `>>=` would give us `IO b`.

bar :: IO ()
bar = getLine >>= \s ->
      getLine >>= \t ->
      putStrLn (s ++ t)
