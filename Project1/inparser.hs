module Inparser where
    
    import Prelude hiding ( lookup )
    import Graph
    import Data.Map ( Map, adjust, fromList, lookup )
    import Data.Maybe ( fromJust ) 

    -- | Recursively split a string into substrings and return them in a list. 
    -- Separate strings on the newline character ('\\n')
    splitLines :: String -> String -> [String] -> [String]
    splitLines []        s ys = ys ++ if s == "" then [] else [s]   -- only add s to our lines list if it is not empty
    splitLines ('\n':xs) s ys = splitLines xs ""    (ys ++ [s])     -- new line, so we add s to our lines list and continue constructing the next line
    splitLines (x:xs)    s ys = splitLines xs (x:s) ys              -- ! we reverse the line here because of (x:s) and not (x ++ [s])

    -- | Recursively split a string into substrings and return them in a list. 
    -- Separate strings on the semicolon character (';') and ignore spaces (' ')
    splitArguments :: String -> String -> [String] -> [String]
    splitArguments []       s ys = ys ++ if s == "" then [] else [s]    -- only add s to our arguments list if it is not empty
    splitArguments (' ':xs) s ys = splitArguments xs s     ys           -- ignore spaces
    splitArguments (';':xs) s ys = splitArguments xs ""    (ys ++ [s])  -- new argument, so we add s to our arguments list and continue constructing the next arg   
    splitArguments (x:xs)   s ys = splitArguments xs (x:s) ys           -- ! we reverse the argument here because of (x:s) and not (x ++ [s])   

    -- | Split a string into lists of substrings by first splitting on the newline character ('\\n') using 'splitLines', 
    -- then splitting the resulting strings on the semicolon character (';') using 'splitArguments'
    parseArguments :: String -> [[String]]
    parseArguments s = Prelude.map (\x -> splitArguments x [] []) (splitLines s [] [])

    -- | Creates an init 'Map' for the in- and outEdge Maps: maps every vertex to an empty list (that will later possibly be filled with edges)
    initMap :: [Vertex] -> Map Vertex [Edge]
    initMap vs = fromList [ (v, []) | v <- vs ]

    -- | Convert a 'String' representation of a graph into a 'Graph' object
    parseInputFile :: String -> Graph
    parseInputFile str = addEdgesSourceTarget $
                          Graph vertices edges 
                            (EdgeData
                                (fromList $ zip edges sources)
                                (fromList $ zip edges targets)
                                (fromList $ zip edges labels)
                            )
                            (VertexData
                                (initMap vertices)
                                (initMap vertices)
                            )
                            where (v:e:s:t:l:_) = parseArguments str
                                  vertices = parseVertices v []
                                  edges    = parseEdges    e []
                                  sources  = parseVertices s []
                                  targets  = parseVertices t []
                                  labels   = l

    -- | Converts a list of strings (vertex ids) into vertices                        
    parseVertices :: [String] -> [Vertex] -> [Vertex]
    parseVertices []     vs = vs
    parseVertices (x:xs) vs = parseVertices xs (vs ++ [x])

    -- | Converts a list of strings (edge ids) into edges
    parseEdges :: [String] -> [Edge] -> [Edge]
    parseEdges []     es = es
    parseEdges (x:xs) es = parseEdges xs (es ++ [x])

    -- | Given a 'Graph' with a complete vertex, edge, sources, targets and labels list, add the in- and outEdges 'Map's for every vertex
    addEdgesSourceTarget :: Graph -> Graph
    addEdgesSourceTarget (Graph vertices edges edgeData@(EdgeData sources targets _) (VertexData outEdges inEdges)) 
        = Graph vertices edges edgeData (VertexData (addVertexEdges edges sources outEdges) (addVertexEdges edges targets inEdges))

    -- | Given a list of edges, a source or target map and a vertex-to-in-or-outedges, returns a map from vertices to in- or outedges
    addVertexEdges :: [Edge] -> Map Edge Vertex -> Map Vertex [Edge] -> Map Vertex [Edge]
    addVertexEdges []     _  m = m
    addVertexEdges (e:es) st m = addVertexEdges es st (addEdgeToVertex (fromJust $ lookup e st) e m)

    -- | Append an 'Edge' to a vertex-to-in-or-outedges-map
    addEdgeToVertex :: Vertex -> Edge -> Map Vertex [Edge] -> Map Vertex [Edge]
    addEdgeToVertex v e m = adjust (e:) v m
