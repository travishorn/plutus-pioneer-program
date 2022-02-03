module Week04.Monad where

-- (>>=)      :: IO a            -> (a -> IO b)            -> IO b
-- bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
-- bindEither :: Either String a -> (a -> Either String b) -> Either String b
-- bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b
--
-- return              :: a -> IO a
-- Just                :: a -> Maybe a
-- Right               :: a -> Either String a
-- (\a -> Writer a []) :: a -> Writer a

-- In Maybe.hs, Either.hs, and Writer.hs, we implemented a monad that takes 3
-- Ints and returns the sum. We can implement this monad once here and then
-- use it to simplify all three files
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s

-- do notation. This does the same as the first example, just with some
-- syntactic sugar.
threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
