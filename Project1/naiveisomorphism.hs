module NaiveIsomorphism where
    import Prelude hiding ( lookup )
    import Graph
    import Data.Maybe
    import Data.Map
    import Data.List hiding ( lookup )

    type VertexPairing = Map Vertex Vertex

    -- Step 1: Make all possible combinations of vertex pairing. If one of these is `correct', then the graphs are isomorphic
    -- Step 2: For all vertex pairings do the following:
    --          For each 2 pairs of vertices do the following:
    --              For every edge from v1 to v2 (in graph 1): check if there is an edge with the same label from w1 to w2 (in graph 2)
    --              Now remove these edges from the edge lists to consider, and continue.

    -- | Returns `True' if the two given graphs are isomorphic
    areGraphsIsomorphic :: Graph -> Graph -> Bool
    areGraphsIsomorphic g1@(Graph vs1 es1 _ _) g2@(Graph vs2 es2 _ _) 
        = length vs1 == length vs2 && 
          length es1 == length es2 && 
          checkVertexPairings g1 g2 (makeAllPossibleVertexPairings vs1 vs2)

    -- | Returns a list of all possible vertex pairings of two given lists
    makeAllPossibleVertexPairings :: [Vertex] -> [Vertex] -> [VertexPairing]
    makeAllPossibleVertexPairings vs1 vs2 = Prelude.map (fromList . zip vs1) (permutations vs2)

    -- | Check all possible vertex pairings until finding one that works, i.e. is isomorphic
    checkVertexPairings :: Graph -> Graph -> [VertexPairing] -> Bool
    checkVertexPairings _  _  []       = False
    checkVertexPairings g1 g2 (vp:vps) = checkVertexPairing g1 g2 vp || checkVertexPairings g1 g2 vps

    -- | Check if a specific vertex pairing is isomorphic
    checkVertexPairing :: Graph -> Graph -> VertexPairing -> Bool
    checkVertexPairing g1 g2 vps = foldrWithKey (\key value r -> r && checkVertexPair g1 g2 vps key value) True vps

    -- | Check for a specific vertex pair if they are isomorphic according to a given pairing
    checkVertexPair :: Graph -> Graph -> VertexPairing -> Vertex -> Vertex -> Bool
    checkVertexPair g1@(Graph _ _ _ (VertexData i1 o1)) 
                    g2@(Graph _ _ _ (VertexData i2 o2)) 
                    vp v1 v2 
                    = let inEdges1 = fromJust $ lookup v1 i1
                          inEdges2 = fromJust $ lookup v2 i2
                          outEdges1 = fromJust $ lookup v1 o1
                          outEdges2 = fromJust $ lookup v2 o2 in
                      checkOutEdges g1 g2 vp v1 v2 outEdges1 outEdges2 && 
                      checkInEdges g1 g2 vp v1 v2 inEdges1 inEdges2

    -- | Given two lists of in-edges, check if they are isomorphic. Returns `True' if we can match every edge in graph 1 to an edge in graph 2 between two vertices
    checkInEdges :: Graph -> Graph -> VertexPairing -> Vertex -> Vertex -> [Edge] -> [Edge] -> Bool
    checkInEdges (Graph _ _ (EdgeData s1 _ l1) _) (Graph _ _ (EdgeData s2 _ l2) _) = checkEdges s1 l1 s2 l2

    -- | Given two lists of out-edges, check if they are isomorphic. Returns `True' if we can match every edge in graph 1 to an edge in graph 2 between two vertices
    checkOutEdges :: Graph -> Graph -> VertexPairing -> Vertex -> Vertex -> [Edge] -> [Edge] -> Bool
    checkOutEdges (Graph _ _ (EdgeData _ t1 l1) _) (Graph _ _ (EdgeData _ t2 l2) _) = checkEdges t1 l1 t2 l2

    -- | Given two lists of edges, check if they are isomorphic. Returns `True' if we can match every edge in graph 1 to an edge in graph 2 between two vertices
    checkEdges :: Map Edge Vertex -> Map Edge Label -> Map Edge Vertex -> Map Edge Label -> VertexPairing -> Vertex -> Vertex -> [Edge] -> [Edge] -> Bool
    checkEdges _ _ _ _ _ _ _ [] [] = True
    checkEdges _ _ _ _ _ _ _ (_:_) [] = False
    checkEdges _ _ _ _ _ _ _ [] (_:_) = False
    checkEdges mv1 ml1 mv2 ml2 vertexpairing v1 v2 (e1:es1) es2 
        = let label1 = fromJust $ lookup e1 ml1
              target1 = fromJust $ lookup e1 mv1
              target2 = fromJust $ lookup target1 vertexpairing
              e2 = findEdgeWithLabelAndVertex mv2 ml2 label1 target2 es2 in
          isJust e2 &&   -- Make sure we can find an edge e2 (=> e2 is not Nothing)
          checkEdges mv1 ml1 mv2 ml2 vertexpairing v1 v2 es1 (Data.List.delete (fromJust e2) es2)

    -- | Given an edge-vertex map, an edge-label map, a label and a vertex in graph2, find an edge in the given list that matches this label and vertex (according to the maps)
    findEdgeWithLabelAndVertex :: Map Edge Vertex -> Map Edge Label -> Label -> Vertex -> [Edge] -> Maybe Edge
    findEdgeWithLabelAndVertex _  _  _ _ []     = Nothing
    findEdgeWithLabelAndVertex vm lm l v (e:es) = if fromJust (lookup e lm) == l && 
                                                     fromJust (lookup e vm) == v
                                                    then Just e 
                                                    else findEdgeWithLabelAndVertex vm lm l v es
