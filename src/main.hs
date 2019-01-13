import Transform
import System.Directory
import Control.Monad 
import Data.List 
import Util

articlesDirectory = "articles/"

inputDirectory = "./sources/"
inputArticles = inputDirectory ++ articlesDirectory
inputTemplates = inputDirectory ++ "/templates/"
inputTemplateArticle = inputTemplates ++ "article.html"
inputTemplateIndex = inputTemplates ++ "index.html"

outputDirectory = "./bin/"
outputArticles = outputDirectory ++ articlesDirectory
outputIndex = outputDirectory ++ "index.html"

getAllMarkdown root = do
  all <- listDirectory root
  let filtered = filter (isSuffixOf ".md") all
  let mapped = map (\x -> root ++ x) filtered
  return mapped

rewriteSuffix :: String -> String
rewriteSuffix source = replaceInString source ".md" ".html"

transformArticle template filename = do
  file <- readFile filename
  let transformedArticle = replaceInString template "{{{ARTICLE_CONTENT}}}" (transform file)
  let outfile = (replaceInString (rewriteSuffix filename) inputDirectory outputDirectory)
  writeFile outfile transformedArticle

setupDirectory output = do
  exists <- (doesDirectoryExist output)
  when (exists == True) $ removeDirectoryRecursive output
  createDirectory output

main = do

  putStrLn "[+] Setting Up Output"
  _ <- setupDirectory outputDirectory
  _ <- setupDirectory outputArticles

  putStrLn "[+] Reading Templates"
  indexTemplate <- readFile inputTemplateIndex
  articleTemplate <- readFile inputTemplateArticle

  putStrLn "[+] Copying Statics"

  putStrLn "[+] Generating Index"
  _ <- writeFile outputIndex indexTemplate

  putStrLn "[+] Converting Articles"

  all <- (getAllMarkdown inputArticles)
  mapM (transformArticle articleTemplate) all

  putStrLn "[+] Generating Lists"

  putStrLn "[+] Done"

--
-- main = do
--  source <- input
--  putStrLn (transform source)
