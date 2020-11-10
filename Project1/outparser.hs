module Outparser where

    import Prelude hiding ( lookup )
    import Graph

    -- | Variable that determines whether to display the vertex ids. If 'removeVertexIds' is 'True', the labels will not be displayed.
    removeVertexIds :: Bool
    removeVertexIds = True

    -- | Given a 'Graph', returns a 'String' representation of the 'Graph' in the .dot style.
    graphToString :: Graph -> String
    graphToString g = graphToStringAux g "\n}\n}"

    -- | Auxilary method for 'graphToString', recursively storing string representations in 'str' firstly of the edges, then all vertices.
    graphToStringAux :: Graph -> String -> String
    graphToStringAux (Graph [] [] _ _) str = "digraph G {\nnode [shape=circle width=0.5" ++ 
                                                    (if removeVertexIds 
                                                        then " label=\"\""
                                                        else "") ++ 
                                                    "]\n{\n\t" ++
                                                    str
    graphToStringAux (Graph (v:vs) [] ed vd) str = graphToStringAux (Graph vs [] ed vd) (vertexToStringLine v ++ str)
    graphToStringAux (Graph vs (e:es) ed vd) str = graphToStringAux (Graph vs es ed vd) (edgeToStringLine e ed ++ str)

    -- | Converts a 'Vertex' into a string with its id and a semicolon
    vertexToStringLine :: Vertex -> String
    vertexToStringLine v = getVertexId v ++ ";"

    -- | Converts an 'Edge' into a string line using a source and target 'Map' from the corresponding 'Graph'
    edgeToStringLine :: Edge -> EdgeData -> String
    edgeToStringLine e (EdgeData s t l) = "\n\t" ++ (getVertexId $ getEdgeVertex e s) ++ 
                                            " -> " ++ (getVertexId $ getEdgeVertex e t) ++ 
                                            " [label=\"" ++ (getEdgeLabel' e l) ++ "\"];"
