import Prelude hiding ( lookup )
import System.IO ( openFile, hGetContents, IOMode(ReadWriteMode) )
import Inparser ( parseInputFile )
import Outparser ( graphToString )
import NaiveIsomorphism ( areGraphsIsomorphic )
import Overlap

inputFileName :: Int -> String
inputFileName n = "input/test" ++ show n ++ ".txt"

outputFileName :: Int -> String
outputFileName n = "output/testout" ++ show n ++ ".dot"

main :: IO ()
main = do
    let v = ["v1", "v2", "v3", "v4", "v5", "v6"]
    let w = ["w1", "w2", "w4", "w3", "w5", "w6"]
    putStrLn $ show $ length $ Overlap.options v w 
    putStrLn $ show $ noOfOptions (length v) (length w)
    return ()

main2 :: IO ()
main2 = do
    handle <- openFile (inputFileName 1) ReadWriteMode ;
    contents <- hGetContents handle ;
    handle2 <- openFile (inputFileName 2) ReadWriteMode ;
    contents2 <- hGetContents handle2 ;
    let graph1 = parseInputFile contents ;
    let graph2 = parseInputFile contents2 ;
    writeFile (outputFileName 1) (show $ areGraphsIsomorphic graph2 graph1)
    writeFile (outputFileName 2) (graphToString graph1)
    writeFile (outputFileName 3) (graphToString graph2)
    return()
