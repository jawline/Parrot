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

getAllMarkdown root = do
  all <- listDirectory root
  let filtered = filter (isSuffixOf ".md") all
  let mapped = map (\x -> root ++ x) filtered
  return mapped

rewriteSuffix :: String -> String
rewriteSuffix source = replaceInString source (".md",".html")

emitArticle :: String -> String -> Int -> (Int, FilePath) -> IO ArticleInfo 
emitArticle oc template total (index, filename) = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ filename)
  source <- readFile filename
  let (article, (title, intro, date, tags)) = transformArticle template source
  let outfile = oc ++ titleToFilename title ++ ".html" 
  writeFile outfile article
  return (title, intro, date, tags)

setupDirectory output = do
  exists <- (doesDirectoryExist output)
  when (exists == True) $ removeDirectoryRecursive output
  createDirectory output

formatListReplacements :: ArticleInfo -> [StringReplacer]
formatListReplacements (title, intro, date, tags) =
  [("{{{LI_NAME}}}", title),
   ("{{{LI_DESCRIPTION}}}", intro),
   ("{{{LI_DATE}}}", showTime date),
   ("{{{LI_TAGS}}}", mergeTags tags),
   ("{{{LI_TARGET}}}", "/" ++ articlesDirectory ++ (titleToFilename title))]

formatListItem :: String -> ArticleInfo -> String
formatListItem template item = multiReplaceInString template replacements
  where
    replacements = formatListReplacements item

writeList :: String -> Int -> (Int, String) -> [ArticleInfo] -> String -> String -> IO ()
writeList outputLists total (index, listname) listitems template itemTemplate = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ listname)
  writeFile (outputLists ++ listname ++ ".html") withContent 
    where
      withTitle = replaceInString template ("{{{LIST_TITLE}}}",listname)
      articleDate (_, _, date, _) = date
      sortedItems = reverse (sortOn articleDate listitems)
      formattedItems = (map (formatListItem itemTemplate) sortedItems)
      withContent = replaceInString withTitle ("{{{LIST_CONTENT}}}",(foldr (++) "" formattedItems))

templateWithNav navTemplate filename = do
  template <- readFile filename
  return (replaceInString template ("{{{NAV_BAR_CONTENT}}}",navTemplate))

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
  articleInfo <- mapM (emitArticle (outputArticles outputDirectory) articleTemplate (length all)) (indexed all)

  putStrLn "[+] Generating Lists"

  let articleTags (_, _, _, tags) = tags
  let listNames = unique (foldr (\l1 r1 -> (articleTags l1) ++ r1) [] articleInfo)

  _ <- mapM_ (\(i, x) -> writeList (outputLists outputDirectory) (length listNames) (i, x) (filter (\y -> elem x (articleTags y)) articleInfo) listTemplate listItemTemplate) (indexed listNames)

  putStrLn "[+] Done"
