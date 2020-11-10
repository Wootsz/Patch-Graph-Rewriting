module Graph2 where
    import Prelude hiding ( lookup )
    import Data.Map
    
    type Vertex = String
    data Edge = Edge { 
        edgeId :: String, 
        edgeSource :: Vertex, 
        edgeTarget :: Vertex, 
        edgeLabel :: String 
    }

    instance Eq Edge where
        (Edge id1 _ _ _) == (Edge id2 _ _ _) = id1 == id2
    
    instance Ord Edge where
        (Edge id1 _ _ _) `compare` (Edge id2 _ _ _) = id1 `compare` id2

    data Graph = Graph {
        vertices :: [Vertex],
        edges :: [Edge]
    }
