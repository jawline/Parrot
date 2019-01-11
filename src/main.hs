import System.IO.Error (tryIOError)
import Transform
import System.Directory
import Control.Monad 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Class 
import Data.List 

getAllMarkdown root = do
  all <- listDirectory root
  let filtered = filter (isSuffixOf ".md") all
  let mapped = map (\x -> root ++ x) filtered
  return mapped

input :: IO String
input = do
  c <- tryIOError getChar
  case c of
    Right(c) -> do
      remain <- input
      return (c:remain)
    Left(_) -> do
      return []

transformFile file = do
  file <- readFile file 
  return (transform file)

printIO :: IO String -> IO ()
printIO ip = do
  str <- ip
  print str

main = do all <- (getAllMarkdown "./test/")
          transformed <- mapM (transformFile) all
          mapM_ (print) transformed

--
-- main = do
--  source <- input
--  putStrLn (transform source)
