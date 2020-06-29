module ImageTemplates where
import System.FilePath
import System.Directory
import Data.Char (toLower)

data ExpectedSize = Original
  | Fixed Float Float
  | Scaled Float
  | ScaledToWidth Float
  | ScaledToHeight Float
  deriving Show

data ImageExpectation = ImageExpectation {
  name :: String,
  size :: ExpectedSize
}

substitutes :: Char -> Char
substitutes ' ' = '_'
substitutes '.' = '.'
substitutes x = toLower x

imageExpectationFilename :: ImageExpectation -> FilePath
imageExpectationFilename expectation = (name expectation) ++ "_" ++ (map substitutes (show (size expectation)))

type ImageExpectations = [String]
type Images = [FilePath]

transformImages :: FilePath -> ImageExpectations -> IO ()
transformImages imageSource expectations = do
  let a = Original
  let b = Scaled 0.3
  putStrLn (imageExpectationFilename ImageExpectation { name="hello", size=a })
  putStrLn (imageExpectationFilename ImageExpectation { name="bye", size=b })
  return ()
