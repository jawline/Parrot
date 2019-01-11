import Transform
import System.Directory
import Control.Monad 
import Data.List 
import Util

articlesDirectory = "articles/"

inputDirectory = "./sources/"
inputArticles = inputDirectory ++ articlesDirectory

outputDirectory = "./bin/"
outputArticles = outputDirectory ++ articlesDirectory

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

  putStrLn "[+] Setting Up Output"
  _ <- setupDirectory outputDirectory
  _ <- setupDirectory outputArticles

  putStrLn "[+] Reading Templates"

  putStrLn "[+] Copying Statics"

  putStrLn "[+] Generating Index"

  putStrLn "[+] Converting Articles"

  all <- (getAllMarkdown inputArticles)
  mapM (transformFile) all

  putStrLn "[+] Generating Lists"

  putStrLn "[+] Done"

--
-- main = do
--  source <- input
--  putStrLn (transform source)
