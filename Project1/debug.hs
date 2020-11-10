import Prelude hiding ( lookup )
import Graph
import Inparser
import Outparser
import Data.Map


-- Debug
printOutEdges :: Graph -> String
printOutEdges (Graph []     _ _ _) = ""
printOutEdges (Graph (v:vs) e ed vd@(VertexData _ i)) = printOutEdges (Graph vs e ed vd) ++ getVertexId v ++ ": " ++ printMaybeEdges (lookup v i) ++ "\n"

printMaybeEdges :: Maybe [Edge] -> String
printMaybeEdges Nothing = ""
printMaybeEdges (Just []) = ""
printMaybeEdges (Just ((e) : es)) = e ++ " " ++ printMaybeEdges (Just es)