module Util where

    -- |  The 'subset' function returns true if the first list is a sub-list
    -- of the second.
    subset :: Ord a => [a] -> [a] -> Bool
    subset = subsetBy compare

    -- |  The 'subsetBy' function is the non-overloaded version of 'subset'.
    subsetBy :: (a -> a -> Ordering) -> [a] -> [a] -> Bool
    subsetBy cmp = loop
        where
            loop [] _ys = True
            loop _xs [] = False
            loop (x:xs) (y:ys) = case cmp x y of
                                    LT -> False
                                    EQ -> loop xs ys
                                    GT -> loop (x:xs) ys

    -- | Returns the sum from 'i' to 'n' of 'f' 'i'.
    sumM :: Num a => Int -> Int -> (Int -> a) -> a
    sumM i n f = f i + if i >= n then 0 else sumM (i+1) n f

    -- | Returns the factorial of an 'Int' n. Assumes n >= 0.
    fac :: Int -> Int 
    fac 0 = 1
    fac n = n * fac (n - 1)

    -- | Returns the binomial coefficient of two 'Int' n, k. Assumes n >= k.
    binCo :: Int -> Int -> Int
    binCo n k = quot (fac n) (fac k * fac (n-k))
