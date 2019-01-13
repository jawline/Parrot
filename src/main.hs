import Transform
import System.Directory
import Control.Monad 
import Data.List 
import Util
import CopyDirectory

articlesDirectory = "articles/"

inputDirectory = "./sources/"
inputArticles = inputDirectory ++ articlesDirectory
inputTemplates = inputDirectory ++ "/templates/"
inputTemplateArticle = inputTemplates ++ "article.html"
inputTemplateIndex = inputTemplates ++ "index.html"
inputStatic = inputDirectory ++ "static/"

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

extractIntroduction :: String -> String
extractIntroduction source = trim intro
  where
    comment = "!=!=!"
    start = comment ++ " Intro: Start"
    end = comment ++ " Intro: End"
    portion = untilString "!=!=! Intro: End" (fromString "!=!=! Intro: Start" source)
    intro = drop (length start) (reverse (drop (length end) (reverse portion))) 

transformArticle :: String -> FilePath -> IO (String, String, String)
transformArticle template filename = do
  source <- readFile filename 
  let transformedArticle = replaceInString template "{{{ARTICLE_CONTENT}}}" (transform source)
  let outfile = (replaceInString (rewriteSuffix filename) inputDirectory outputDirectory)
  writeFile outfile transformedArticle
  return ("", "", extractIntroduction source)

setupDirectory output = do
  exists <- (doesDirectoryExist output)
  when (exists == True) $ removeDirectoryRecursive output
  createDirectory output

title :: (String, String, String) -> String
title (x, _, _) = x

date :: (String, String, String) -> String
date (_, x, _) = x

intro :: (String, String, String) -> String
intro (_, _, x) = x

main = do

  putStrLn "[+] Setting Up Output"
  _ <- setupDirectory outputDirectory
  _ <- setupDirectory outputArticles

  putStrLn "[+] Reading Templates"
  indexTemplate <- readFile inputTemplateIndex
  articleTemplate <- readFile inputTemplateArticle

  putStrLn "[+] Copying Statics"
  copyDirectory inputStatic outputDirectory

  putStrLn "[+] Generating Index"
  _ <- writeFile outputIndex indexTemplate

  putStrLn "[+] Converting Articles"

  all <- (getAllMarkdown inputArticles)
  articleInfo <- mapM (transformArticle articleTemplate) all

  mapM_ (\x -> print (title x)) articleInfo
  mapM_ (\x -> print (date x)) articleInfo
  mapM_ (\x -> print (intro x)) articleInfo

  putStrLn "[+] Generating Lists"

  putStrLn "[+] Done"
