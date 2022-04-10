import MTree
import MParser
import MConstraints

import System.IO

import Prelude

import System.Environment
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)


main :: IO ()
main = do
    args <- getArgs
    doAssign (args!!0) (args!!1)
    
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

doAssign :: String -> String -> IO ()
doAssign inName outName = do
    pResult <- mainParse inName

    if pResult == 1 
        then do
            tree <- makeTree inName
            let t2s = treeToString tree ""
            createAndWriteFile outName t2s
    else if pResult == 0
        then createAndWriteFile outName ("Error while parsing input file")
    else if pResult == (-1)
        then createAndWriteFile outName ("invalid machine/task")
    else if pResult == (-2)
        then createAndWriteFile outName ("partial assignment error")
    else if pResult == (-3)
        then createAndWriteFile outName ("machine penalty error")
    else if pResult == (-4)
        then createAndWriteFile outName ("invalid penalty")
    else if pResult == (-5)
        then createAndWriteFile outName ("invalid task")
    else print ("")