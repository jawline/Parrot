import Transform
import System.Directory
import Control.Monad 
import Data.List 
import Util
import CopyDirectory

articlesDirectory = "articles/"
listsDirectory = "list/"

inputDirectory = "./sources/"
inputArticles = inputDirectory ++ articlesDirectory
inputTemplates = inputDirectory ++ "/templates/"
inputTemplateArticle = inputTemplates ++ "article.html"
inputTemplateList = inputTemplates ++ "list.html"
inputTemplateNav = inputTemplates ++ "nav.html"
inputTemplateListItem = inputTemplates ++ "list_item.html"
inputTemplateIndex = inputTemplates ++ "index.html"
inputStatic = inputDirectory ++ "static/"

outputDirectory = "./bin/"
outputArticles = outputDirectory ++ articlesDirectory
outputLists = outputDirectory ++ listsDirectory
outputIndex = outputDirectory ++ "index.html"

getAllMarkdown root = do
  all <- listDirectory root
  let filtered = filter (isSuffixOf ".md") all
  let mapped = map (\x -> root ++ x) filtered
  return mapped

rewriteSuffix :: String -> String
rewriteSuffix source = replaceInString source ".md" ".html"

extractTitle :: String -> String
extractTitle source = trim (drop (length prelude) (findLine prelude source))
  where
    prelude = "!=!=! Title:"

extractDate :: String -> String
extractDate source = trim (drop (length prelude) (findLine prelude source))
  where
    prelude = "!=!=! Created:"

parseTags :: String -> String -> [String]
parseTags current [] = [current]
parseTags current (',':xs) = current:(parseTags "" xs)
parseTags current (x:xs) = parseTags (x:current) xs

extractTags :: String -> [String]
extractTags source = map trim $ map reverse $ tags 
  where
    prelude = "!=!=! Tags:"
    tags = parseTags "" (trim (drop (length prelude) (findLine prelude source)))

extractIntroduction :: String -> String
extractIntroduction source = trim intro
  where
    comment = "!=!=!"
    start = comment ++ " Intro: Start"
    end = comment ++ " Intro: End"
    portion = untilString "!=!=! Intro: End" (fromString "!=!=! Intro: Start" source)
    intro = drop (length start) (reverse (drop (length end) (reverse portion))) 

mergeTag :: String -> String -> String
mergeTag current next = if (length current) == 0 then next else current ++ ", " ++ next

mergeTags tags = foldr mergeTag "" tags

transformArticle :: String -> FilePath -> IO (String, String, String, [String])
transformArticle template filename = do
  source <- readFile filename
  let articleTitle = (extractTitle source) 
  let articleDate = (extractDate source)
  let articleInfo = (extractIntroduction source)
  let articleTags = (extractTags source)
  let withText = replaceInString template "{{{ARTICLE_CONTENT}}}" (transform source)
  let withTitle = replaceInString withText "{{{ARTICLE_TITLE}}}" articleTitle
  let withTime = replaceInString withTitle "{{{ARTICLE_TIME}}}" articleDate
  let withTags = replaceInString withTime "{{{ARTICLE_TAGS}}}" (mergeTags articleTags)
  let transformedArticle = withTags
  let outfile = outputArticles ++ titleToFilename articleTitle ++ ".html" 
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

formatListItem :: String -> (String, String, String, [String]) -> String
formatListItem template item = withTargets 
  where
    withTitle = replaceInString template "{{{LI_NAME}}}" (title item)
    withIntro = replaceInString withTitle "{{{LI_DESCRIPTION}}}" (intro item)
    withDate = replaceInString withIntro "{{{LI_DATE}}}" (date item)
    withTags = replaceInString withDate "{{{LI_TAGS}}}" (mergeTags (tags item))
    withTargets = replaceInString withTags "{{{LI_TARGET}}}" ("/" ++ articlesDirectory ++ (titleToFilename (title item)))

writeList :: String -> [(String, String, String, [String])] -> String -> String -> IO ()
writeList listname listitems template itemTemplate = do
  writeFile (outputLists ++ listname ++ ".html") withContent 
  where
    withTitle = replaceInString template "{{{LIST_TITLE}}}" listname
    formattedItems = (map (formatListItem itemTemplate) listitems)
    withContent = replaceInString withTitle "{{{LIST_CONTENT}}}" (foldr (++) "" formattedItems)

templateWithNav navTemplate filename = do
  template <- readFile filename
  return (replaceInString template "{{{NAV_BAR_CONTENT}}}" navTemplate)

main = do

  putStrLn "[+] Setting Up Output"

  _ <- setupDirectory outputDirectory
  _ <- setupDirectory outputArticles
  _ <- setupDirectory outputLists

  putStrLn "[+] Reading Templates"

  navTemplate <- readFile inputTemplateNav
  indexTemplate <- templateWithNav navTemplate inputTemplateIndex
  articleTemplate <- templateWithNav navTemplate inputTemplateArticle
  listTemplate <- templateWithNav navTemplate inputTemplateList
  listItemTemplate <- readFile inputTemplateListItem

  putStrLn "[+] Copying Statics"

  copyDirectory inputStatic outputDirectory

  putStrLn "[+] Generating Index"

  _ <- writeFile outputIndex indexTemplate

  putStrLn "[+] Converting Articles"

  all <- (getAllMarkdown inputArticles)
  articleInfo <- mapM (transformArticle articleTemplate) all

  putStrLn "[+] Generating Lists"

  let listNames = foldr (\l1 r1 -> (tags l1) ++ r1) [] articleInfo
  _ <- mapM_ (\x -> writeList x (filter (\y -> elem x (tags y)) articleInfo) listTemplate listItemTemplate) listNames

  putStrLn "[+] Done"
