import Transform
import System.Directory
import Control.Monad 
import Data.List 
import Util
import CopyDirectory
import Meta
import Paths

import System.FilePath
import System.Environment
import System.Exit

getAllMarkdown root = do
  all <- listDirectory root
  return (mapper (mdfilter all))
  where
    mdfilter = filter (isSuffixOf ".md")
    mapper = map (\x -> root </> x)

rewriteSuffix :: String -> String
rewriteSuffix source = replaceInString source (".md",".html")

emitArticle :: FilePath -> String -> Int -> (Int, FilePath) -> IO ArticleInfo 
emitArticle outputDir template total (index, filename) = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ filename)
  source <- readFile filename
  let (article, (title, intro, date, tags)) = transformArticle template source
  let outfile = outputDir </> titleToFilename title <.> "html" 
  writeFile outfile article
  return (title, intro, date, tags)

setupDirectory output = do
  exists <- (doesDirectoryExist output)
  when (exists == True) $ removeDirectoryRecursive output
  createDirectory output

writeList :: String -> Int -> (Int, String) -> [ArticleInfo] -> String -> String -> IO ()
writeList outputLists total (index, listname) listitems template itemTemplate = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ listname)
  writeFile (outputLists ++ listname ++ ".html") (multiReplaceInString template replacers)
    where
      articleDate (_, _, date, _) = date
      sortedItems = reverse (sortOn articleDate listitems)
      formattedItems = (map (transformListItem itemTemplate) sortedItems)
      replacers = [("{{{LIST_TITLE}}}",listname),("{{{LIST_CONTENT}}}",(foldr (++) "" formattedItems))]

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
