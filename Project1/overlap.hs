module Overlap where
    import Graph
    import Prelude hiding ( lookup )
    import Util ( sumM, fac, binCo )

    type VertexMatching = [(Vertex, Vertex)]

    -- | Returns all possible vertex matchings of two vertex sets.
    options :: [Vertex] -> [Vertex] -> [VertexMatching]
    options [] _ = [[]]
    options _ [] = [[]]
    options (v:vs) (w:ws) = options vs (w:ws) ++ concatMap (\option -> map (: option) vPairings) furtherOptions
                            where vPairings = pairVWithAny v (w:ws)
                                  furtherOptions = options vs ws

    -- | Returns the given vertex v paired with all vertices from the given vertex list.
    pairVWithAny :: Vertex -> [Vertex] -> [(Vertex, Vertex)]
    pairVWithAny _ [] = []
    pairVWithAny v (w:ws) = (v, w) : pairVWithAny v ws

    -- | Returns the amount of vertex matching possibilities for vertex lists of length 'n' and 'm'. Uses 'noOfOptions''
    noOfOptions :: Int -> Int -> Int
    noOfOptions n m = if n > m then noOfOptions' m n else noOfOptions' n m

    -- | Returns the amount of vertex matching possibilities for vertex lists of length 'n' and 'm'. Assumes 'n' <= 'm' 
    noOfOptions' :: Int -> Int -> Int
    noOfOptions' n m = sumM 0 (min n m) (\k -> (fac n `quot` fac k) * binCo m (n - k))
