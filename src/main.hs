import Transform
import System.Directory
import Control.Monad 
import Data.List 
import Util
import CopyDirectory
import Meta
import Paths

import System.Environment
import System.Exit

type ArticleInfo = (String, Float, String, [String])

getAllMarkdown root = do
  all <- listDirectory root
  let filtered = filter (isSuffixOf ".md") all
  let mapped = map (\x -> root ++ x) filtered
  return mapped

rewriteSuffix :: String -> String
rewriteSuffix source = replaceInString source ".md" ".html"

transformArticle :: String -> String -> Int -> (Int, FilePath) -> IO ArticleInfo 
transformArticle oc template total (index, filename) = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ filename)
  source <- readFile filename
  let articleTitle = (extractTitle source) 
  let articleDate = (extractDate source)
  let articleInfo = (extractIntroduction source)
  let articleTags = (extractTags source)
  let withText = replaceInString template "{{{ARTICLE_CONTENT}}}" (transform source)
  let withTitle = replaceInString withText "{{{ARTICLE_TITLE}}}" articleTitle
  let withTime = replaceInString withTitle "{{{ARTICLE_TIME}}}" (showTime articleDate)
  let withTags = replaceInString withTime "{{{ARTICLE_TAGS}}}" (mergeTags articleTags)
  let transformedArticle = withTags
  let outfile = oc ++ titleToFilename articleTitle ++ ".html" 
  writeFile outfile transformedArticle
  return (articleTitle, articleDate, extractIntroduction source, extractTags source)

setupDirectory output = do
  exists <- (doesDirectoryExist output)
  when (exists == True) $ removeDirectoryRecursive output
  createDirectory output

title (x, _, _, _) = x
date (_, x, _, _) = x
intro (_, _, x, _) = x
tags (_, _, _, x) = x

formatListItem :: String -> ArticleInfo -> String
formatListItem template item = withTargets 
  where
    withTitle = replaceInString template "{{{LI_NAME}}}" (title item)
    withIntro = replaceInString withTitle "{{{LI_DESCRIPTION}}}" (intro item)
    withDate = replaceInString withIntro "{{{LI_DATE}}}" (showTime (date item))
    withTags = replaceInString withDate "{{{LI_TAGS}}}" (mergeTags (tags item))
    withTargets = replaceInString withTags "{{{LI_TARGET}}}" ("/" ++ articlesDirectory ++ (titleToFilename (title item)))

writeList :: String -> Int -> (Int, String) -> [ArticleInfo] -> String -> String -> IO ()
writeList outputLists total (index, listname) listitems template itemTemplate = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ listname)
  writeFile (outputLists ++ listname ++ ".html") withContent 
    where
      withTitle = replaceInString template "{{{LIST_TITLE}}}" listname
      sortedItems = reverse (sortOn date listitems)
      formattedItems = (map (formatListItem itemTemplate) sortedItems)
      withContent = replaceInString withTitle "{{{LIST_CONTENT}}}" (foldr (++) "" formattedItems)

templateWithNav navTemplate filename = do
  template <- readFile filename
  return (replaceInString template "{{{NAV_BAR_CONTENT}}}" navTemplate)

failArguments = do
  putStrLn "Incorrect Usage"
  putStrLn "Expected MDHS Source Output"
  exitWith (ExitFailure 1)

main = do

  putStrLn "[+] Finding Targets"

  programArguments <- getArgs

  when (length programArguments /= 2) $ failArguments

  let inputDirectory  = programArguments !! 0
  let outputDirectory = programArguments !! 1

  putStrLn "[+] Setting Up Output"

  _ <- setupDirectory outputDirectory
  _ <- setupDirectory (outputArticles outputDirectory)
  _ <- setupDirectory (outputLists outputDirectory)

  putStrLn "[+] Reading Templates"

  navTemplate <- readFile (inputTemplateNav inputDirectory)
  indexTemplate <- templateWithNav navTemplate (inputTemplateIndex inputDirectory)
  articleTemplate <- templateWithNav navTemplate (inputTemplateArticle inputDirectory)
  listTemplate <- templateWithNav navTemplate (inputTemplateList inputDirectory)
  listItemTemplate <- readFile (inputTemplateListItem inputDirectory)

  putStrLn "[+] Copying Statics"

  copyDirectory (inputStatic inputDirectory) (outputDirectory)

  putStrLn "[+] Generating Index"

  _ <- writeFile (outputIndex outputDirectory) indexTemplate

  putStrLn "[+] Converting Articles"

  all <- getAllMarkdown (inputArticles inputDirectory)
  articleInfo <- mapM (transformArticle (outputArticles outputDirectory) articleTemplate (length all)) (indexed all)

  putStrLn "[+] Generating Lists"

  let listNames = unique (foldr (\l1 r1 -> (tags l1) ++ r1) [] articleInfo)
  _ <- mapM_ (\(i, x) -> writeList (outputLists outputDirectory) (length listNames) (i, x) (filter (\y -> elem x (tags y)) articleInfo) listTemplate listItemTemplate) (indexed listNames)

  putStrLn "[+] Done"
