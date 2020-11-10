module Isomorphism where
    import Graph
    import Prelude hiding ( lookup )
    import Data.Map
    import Data.Maybe

    type VertexPairing = Map Vertex Vertex

    areVerticesIsomorphic :: Vertex -> Vertex -> Graph -> Graph -> VertexPairing -> Bool
    areVerticesIsomorphic v1 v2 (Graph _ _ _ (VertexData i1 o1)) (Graph _ _ _ (VertexData i2 o2)) vp = False

    initPairing :: Map Vertex Vertex
    initPairing = fromList []

    checkOutEdges :: Vertex -> Vertex -> Graph -> Graph -> VertexPairing -> Bool
    checkOutEdges v1 v2 
        (Graph _ _ _ (VertexData _ o1)) 
        (Graph _ _ _ (VertexData _ o2)) vp = False
                                                where outEdges1 = lookup v1 o1
                                                      outEdges2 = lookup v2 o2

    existsMatchingEdge :: Vertex -> Vertex -> Graph -> Graph -> VertexPairing -> [Edge] -> [Edge] -> Bool
    existsMatchingEdge _ _ _ _ _ _ [] = False
    existsMatchingEdge _ _ _ _ _ (e:es) (e2:es2) = False



    -- | Given a label and a label function (from edge to labels), find all edges in the given list with the given label
    findEdgesWithSameLabel :: String ->  Map Edge String -> [Edge] -> [Edge] 
    findEdgesWithSameLabel l m e = findEdgesWithSameLabel' l m e []

    findEdgesWithSameLabel' :: String ->  Map Edge String -> [Edge] -> [Edge] -> [Edge]
    findEdgesWithSameLabel' _     _        []     edgesSoFar = edgesSoFar
    findEdgesWithSameLabel' label labelMap (e:es) edgesSoFar = (if (fromJust $ lookup e labelMap) == label then [e] else []) ++ findEdgesWithSameLabel' label labelMap es edgesSoFar



    -- | Compare the vertex sets in both graphs. Returns 'True' if they are the same size.
    haveEqualVertexSize :: Graph -> Graph -> Bool
    haveEqualVertexSize (Graph vs1 _ _ _) (Graph vs2 _ _ _) = length vs1 == length vs2

    -- | Compare the edge sets in both graphs. Returns 'True' if they are the same size.
    haveEqualEdgeSize :: Graph -> Graph -> Bool
    haveEqualEdgeSize (Graph _ es1 _ _) (Graph _ es2 _ _) = length es1 == length es2