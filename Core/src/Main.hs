import Transform
import System.Directory
import Control.Monad
import Data.List
import Util
import CopyDirectory
import Meta
import Paths
import Templates (rewriteTemplates)
import ConstantTemplates (constantRewrite, constantStaticTemplates)
import ImageTemplates (transformImages, rewriteImageTemplates, ImageExpectation)
import ContentTemplate (rewriteContentTemplates)
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

emitArticle :: OutputDirectories -> String -> Int -> (Int, FilePath) -> IO (ArticleInfo, [ImageExpectation])
emitArticle output template total (index, filename) = do

  source <- readFile filename
  let (source', _) = rewriteContentTemplates (relativeArticles output) (relativeLists output) source
  let (source'', imageExpectations) = rewriteImageTemplates (relativeImages output) source'
  let (article, info) = transformArticle template source''
  let outfile = (articles output) </> titleToFilename (articleTitle info) <.> "html"
  writeFile outfile article

  putStrLn $ "[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ filename ++ " dependencies: " ++ (show imageExpectations)
  return (info, imageExpectations)

setupDirectory output = do
  exists <- (doesDirectoryExist output)
  when (not exists) $ (createDirectory output)

writeListReplacers title content = [
  (constantRewrite "LIST_TITLE" title),
  (constantRewrite "LIST_CONTENT" content)]

articleTime article = asUTCTime (articleDate article)

writeList :: String -> Int -> (Int, String) -> [ArticleInfo] -> String -> String -> IO ()
writeList outputLists total (index, listname) listitems template itemTemplate = do
  putStrLn ("[" ++ (show (index + 1)) ++ " of " ++ (show total) ++ "] " ++ listname)
  writeFile (outputLists </> listname <.> ".html") rewritten
    where
      sortedItems = reverse (sortOn articleTime listitems)
      formattedItems = map (transformListItem itemTemplate) sortedItems
      replacers = writeListReplacers listname (foldr (++) "" formattedItems)
      rewritten = constantStaticTemplates replacers template

templateBase output filename = do
  template <- readFile filename
  let (template', _) = rewriteContentTemplates (relativeArticles output) (relativeLists output) template
  return (rewriteImageTemplates (relativeImages output) template')

templateWithNav output navTemplate filename = do
  (template, imgs) <- templateBase output filename
  return (constantStaticTemplates rewriteRules template, imgs)
  where
    rewriteRules = [(constantRewrite "NAV_BAR_CONTENT" navTemplate)]

allTemplates input output = do
  (navTemplate, navImg) <- templateBase output (templateNav templates)
  (indexTemplate, indexImg) <- templateWithNav output navTemplate (templateIndex templates)
  (articleTemplate, articleImg) <- templateWithNav output navTemplate (templateArticle templates)
  (listTemplate, listImg) <- templateWithNav output navTemplate (templateList templates)
  (listItemTemplate, liImg) <- templateBase output (templateListItem templates)
  return ((indexTemplate, articleTemplate, listTemplate, listItemTemplate, navTemplate), navImg ++ indexImg ++ articleImg ++ listImg ++ liImg)
  where
    templates = inputTemplates input

failArguments = do
  putStrLn "Incorrect Usage"
  putStrLn "Expected MDHS Source Output"
  exitWith (ExitFailure 1)

mergeArticleExpectations articleData = foldr (\l r -> l ++ r) [] expected
  where
    expected = map(\(_, exp) -> exp) articleData

transformDirectory input output = do

  putStrLn "[+] Reading Templates"

  ((indexTemplate, articleTemplate, listTemplate, listItemTemplate, navTemplate), expectedImages) <- allTemplates input output

  putStrLn "[+] Copying Statics"

  copyDirectory (inputStatic input) (root output)

  putStrLn "[+] Generating Index"

  _ <- writeFile (index output) indexTemplate

  putStrLn "[+] Converting Articles"

  all <- getAllMarkdown (inputArticles input)
  emitData <- mapM (emitArticle output articleTemplate (length all)) (indexed all)

  let articleInfo = map (\(info, _) -> info) emitData
  let articleImages = mergeArticleExpectations emitData

  putStrLn "[+] Generating Lists"

  let listNames = unique (foldr (\l1 r1 -> (articleTags l1) ++ r1) [] articleInfo)

  _ <- mapM_ (\(i, x) -> writeList (lists output) (length listNames) (i, x) (filter (\y -> elem x (articleTags y)) articleInfo) listTemplate listItemTemplate) (indexed listNames)

  putStrLn "[+] Translating Images"
  let expectedImages' = expectedImages ++ articleImages
  transformImages (inputImages input) (images output) expectedImages'

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
