import Transform
import System.Directory
import Control.Monad
import Data.List
import Util
import CopyDirectory
import Meta
import Paths
import Templates (rewriteTemplates)
import ImageTemplates (transformImages)
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.FilePath
import System.Environment
import System.Exit
import Control.Monad.Trans.State as State
    ( State, put, modify, execState, get )

getAllMarkdown root = do
  putStrLn ("[+] Scanning " ++ root)
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
  when (not exists) $ (createDirectory output)

writeList :: String -> Int -> (Int, String) -> [ArticleInfo] -> String -> String -> IO ()
writeList outputLists total (index, listname) listitems template itemTemplate = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ listname)
  writeFile (outputLists </> listname <.> ".html") (multiReplaceInString template replacers)
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

transformDirectory input output = do
  putStrLn "[+] Reading Templates"

  navTemplate <- readFile (templateNav templates)
  indexTemplate <- templateWithNav navTemplate (templateIndex templates)
  articleTemplate <- templateWithNav navTemplate (templateArticle templates)
  listTemplate <- templateWithNav navTemplate (templateList templates)
  listItemTemplate <- readFile (templateListItem templates)

  putStrLn "[+] Copying Statics"

  copyDirectory (inputStatic input) (root output)

  putStrLn "[+] Generating Index"

  _ <- writeFile (index output) indexTemplate

  putStrLn "[+] Converting Articles"

  all <- getAllMarkdown (inputArticles input)
  articleInfo <- mapM (emitArticle (articles output) articleTemplate (length all)) (indexed all)

  putStrLn "[+] Generating Lists"

  let articleTags (_, _, _, tags) = tags
  let listNames = unique (foldr (\l1 r1 -> (articleTags l1) ++ r1) [] articleInfo)

  _ <- mapM_ (\(i, x) -> writeList (lists output) (length listNames) (i, x) (filter (\y -> elem x (articleTags y)) articleInfo) listTemplate listItemTemplate) (indexed listNames)

  putStrLn "[+] Translating Images"

  transformImages (inputImages input) (images output) []

  putStrLn ("[+] Finished Transforming " ++ (inputRoot input))
  return ()
  where
    templates = (inputTemplates input)

main = do

  putStrLn "[+] Finding Targets"

  programArguments <- getArgs

  when (length programArguments /= 2) $ failArguments

  let input = inputDirectories (programArguments !! 0)
  let output = outputDirectories (programArguments !! 1)

  putStrLn "[+] Setting Up Output"

  _ <- setupDirectory (root output)
  _ <- setupDirectory (images output)
  _ <- setupDirectory (articles output)
  _ <- setupDirectory (lists output)

  _ <- transformDirectory input output

  putStrLn "[+] Done"
