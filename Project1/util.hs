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
