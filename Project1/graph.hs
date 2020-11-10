module Graph where

    import Prelude hiding ( lookup )
    import Data.Map ( Map, lookup )
    import Data.Maybe ( fromJust )

    type Vertex = String
    type Edge = String
    type Label = String

    type EdgeSource = Map Edge Vertex
    type EdgeTarget = Map Edge Vertex
    type EdgeLabel = Map Edge Label

    data EdgeData = EdgeData {
        source :: EdgeSource,
        target :: EdgeTarget,
        label :: EdgeLabel
    }

    type VertexIn = Map Vertex [Edge]
    type VertexOut = Map Vertex [Edge]

    data VertexData = VertexData {
        inEdges :: VertexIn,
        outEdges :: VertexOut
    }

    data Graph = Graph {
        vertices :: [Vertex],
        edges :: [Edge],
        edgeData :: EdgeData,
        vertexData :: VertexData
    }

    type Scheme = (Graph, Graph)

    ----------------------
    -- Vertex functions --
    ----------------------

    -- | Returns the identifier of a vertex.
    -- O(1)
    getVertexId :: Vertex -> String
    getVertexId x = x

    -- | Given a 'Vertex' 'v' and a 'Graph', returns the amount of edges with source 'v'.
    -- O(V)
    getVertexOutDegree :: Vertex -> Graph -> Int
    getVertexOutDegree v (Graph _ _ _ (VertexData _ o)) = getVertexDegree v o

    -- | Given a 'Vertex' 'v' and a 'Graph', returns the amount of edges with target 'v'.
    -- O(V)
    getVertexInDegree :: Vertex -> Graph -> Int
    getVertexInDegree v (Graph _ _ _ (VertexData i _)) = getVertexDegree v i

    -- | Given a 'Vertex' 'v' and a 'Map', returns the amount of edges that map to 'v'.
    -- O(V)
    getVertexDegree :: Vertex -> Map Vertex [Edge] -> Int
    getVertexDegree v m = length $ lookup v m

    -- | Returns all vertices in the given graph that have in-degree 'n'.
    getInDegreeNVertices :: Int -> Graph -> [Vertex]
    getInDegreeNVertices n (Graph vs _ _ (VertexData i _)) = getNDegreeVertices n vs i

    -- | Returns all vertices in the given graph that have out-degree 'n'.
    getOutDegreeNVertices :: Int -> Graph -> [Vertex]
    getOutDegreeNVertices n (Graph vs _ _ (VertexData _ o)) = getNDegreeVertices n vs o

    -- | Given a list of vertices and a map, returns the verttices with degree 'n'.
    -- O(V^2)
    getNDegreeVertices :: Int -> [Vertex] -> Map Vertex [Edge] -> [Vertex]
    getNDegreeVertices n vs m = filter (\v -> n == getVertexDegree v m) vs



    --------------------
    -- Edge functions --
    --------------------

    -- | Returns the identifier of an edge.
    getEdgeId :: Edge -> String
    getEdgeId x = x

    -- | Given an 'Edge' and a 'Graph', returns its source 'Vertex'.
    getEdgeSource :: Edge -> Graph -> Vertex
    getEdgeSource e (Graph _ _ (EdgeData s _ _) _) = getEdgeVertex e s

    -- | Given an 'Edge' and a 'Graph', returns its target 'Vertex'.
    getEdgeTarget :: Edge -> Graph -> Vertex
    getEdgeTarget e (Graph _ _ (EdgeData _ t _) _) = getEdgeVertex e t

    -- | Given an 'Edge' and a 'Map' from edges to vertices, returns the corresponding 'Vertex'.
    getEdgeVertex :: Edge -> Map Edge Vertex -> Vertex
    getEdgeVertex e m = fromJust $ lookup e m

    -- | Given an edge and a 'Graph', returns the corresponding 'String' label.
    getEdgeLabel :: Edge -> Graph -> Label
    getEdgeLabel e (Graph _ _ (EdgeData _ _ l) _) = getEdgeLabel' e l

    -- | Given an edge and a 'Map' from edges to strings, returns the corresponding 'String' label.
    getEdgeLabel' :: Edge -> Map Edge Label -> Label
    getEdgeLabel' e l = fromJust $ lookup e l


