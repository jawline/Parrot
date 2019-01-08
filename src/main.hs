import System.IO.Error (tryIOError)
import Transform

input :: IO String
input = do
  c <- tryIOError getChar
  case c of
    Right(c) -> do
      remain <- input
      return (c:remain)
    Left(_) -> do
      return []

main = do
  source <- input
  putStrLn (transform source)
