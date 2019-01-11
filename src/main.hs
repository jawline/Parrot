import Transform
import System.Directory
import Control.Monad 
import Data.List 
import Util

inputDirectory = "./sources/"
outputDirectory = "./bin/" 

getAllMarkdown root = do
  all <- listDirectory root
  let filtered = filter (isSuffixOf ".md") all
  let mapped = map (\x -> root ++ x) filtered
  return mapped

rewriteSuffix :: String -> String
rewriteSuffix source = replaceInString source ".md" ".html"

transformFile filename = do
  file <- readFile filename
  let outfile = (replaceInString (rewriteSuffix filename) inputDirectory outputDirectory)
  writeFile outfile (transform file)

setupDirectory output = do
  exists <- (doesDirectoryExist output)
  when (exists == True) $ removeDirectoryRecursive output
  createDirectory output

main = do
  setup <- setupDirectory outputDirectory 
  all <- (getAllMarkdown inputDirectory)
  transformed <- mapM (transformFile) all
  mapM_ (print) transformed

--
-- main = do
--  source <- input
--  putStrLn (transform source)
