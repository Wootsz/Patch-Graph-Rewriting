import Prelude hiding ( lookup )
import System.IO ( openFile, hGetContents, IOMode(ReadWriteMode) )
import Inparser ( parseInputFile )
import Outparser ( graphToString )
import NaiveIsomorphism ( areGraphsIsomorphic )

inputFileName :: String
inputFileName = "input/test.txt"

inputFileName2 :: String
inputFileName2 = "input/test2.txt"

outputFileName :: Int -> String
outputFileName n = "output/testout" ++ show n ++ ".dot"

main :: IO ()
main = do
    handle <- openFile inputFileName ReadWriteMode ;
    contents <- hGetContents handle ;
    handle2 <- openFile inputFileName2 ReadWriteMode ;
    contents2 <- hGetContents handle2 ;
    let graph1 = parseInputFile contents ;
    let graph2 = parseInputFile contents2 ;
    writeFile (outputFileName 1) (show $ areGraphsIsomorphic graph2 graph1)
    writeFile (outputFileName 2) (graphToString graph1)
    writeFile (outputFileName 3) (graphToString graph2)
    return()