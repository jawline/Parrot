module ImageTemplates where
import System.FilePath
import System.Directory
import Data.Char (toLower)
import Graphics.Image
import Util (dedup, if')
import Templates (rewriteTemplates, defaultExtractTemplateString)

-- Data type represents the possible transformations for a source image.
data ExpectedSize = Original
  | Fixed Int Int
  | Scaled Float
  | ScaledToWidth Float
  | ScaledToHeight Float
  | QualityThumb
  | QualityHigh
  deriving (Show, Eq)

-- An image 'expectation' includes the expected filename and dimensions or scaling factor
data ImageExpectation = ImageExpectation {
  name :: String,
  fileType :: String,
  size :: ExpectedSize
} deriving (Show, Eq)

-- Character substitutions to go from an expectation to its filename when being translated
substitutes :: Char -> Char
substitutes ' ' = '_'
substitutes '.' = '_'
substitutes x = toLower x

imageExpectationFilename :: ImageExpectation -> FilePath
imageExpectationFilename expectation = ((name expectation) ++ "_" ++ (Prelude.map substitutes (show (size expectation)))) <.> (fileType expectation)

-- Round floating dimensions to integers
roundDims (a, b) = (round a, round b)

-- Scale a integer dimension pair by a scalar
scaleBy (a, b) scalar = roundDims (((fromIntegral a) * scalar), ((fromIntegral b) * scalar))

-- Convert integral dimension to floating dimensions
toFloating (a, b) = (fromIntegral a, fromIntegral b)

minArea (w1, h1) (w2, h2) = if' ((w1 * h1) > (w2 * h2)) (w2, h2) (w1, h1)

makeLargestDimension x dims = minArea scaledWidth scaledHeight
  where
    scaledWidth = decideShape dims (ScaledToWidth x)
    scaledHeight = decideShape dims (ScaledToHeight x)

-- Given the current dimensions and the expected shape calculate the new dimensions
decideShape (a, b) (Fixed x y) = (x, y)
decideShape (a, b) (Scaled x) = roundDims (a' * x, b' * x)
  where (a', b') = toFloating (a, b)
decideShape (a, b) (ScaledToWidth x) = scaleBy (a, b) scalar
  where
    scalar = x / (fromIntegral a)
decideShape (a, b) (ScaledToHeight x) = scaleBy (a, b) scalar
  where
    scalar = x / (fromIntegral b)
decideShape dims QualityThumb = makeLargestDimension 100 dims
decideShape dims QualityHigh = makeLargestDimension 400 dims 

-- Transforms a given file expectation (and source file) to a output image
transformExpectation :: FilePath -> FilePath -> ImageExpectation -> IO ()
transformExpectation imageRoot outputDir expectation = do
  let filename = outputDir </> (imageExpectationFilename expectation)
  putStrLn $ "[+] Transforming " ++ filename
  file <- readImageRGBA VU (imageRoot </> (name expectation) <.> (fileType expectation))
  let newDims = decideShape (dims file) (size expectation)
  writeImage filename $ resize Bilinear Edge newDims file
  return ()

-- Transforms a set of source files into web optimized files using expectations generated by finding the image template strings in markdown files, stopping unused files ending up in the build and automatically converting them to desired resolutions.
transformImages :: FilePath -> FilePath -> [ImageExpectation] -> IO ()
transformImages imageSource imageDest expectations = do
  mapM_ (\x -> transformExpectation imageSource imageDest x) (dedup expectations)
  return ()

imgTemplateStart ('$':'{':'{':'{':'i':'m':'g':':':xs) = Just xs
imgTemplateStart _ = Nothing

nameAndType filePath = (dropExtension filePath, takeExtension filePath)

imgTemplateRewriter :: String -> String -> Maybe (String, [ImageExpectation])
imgTemplateRewriter hostedImagesPath templateString = Just (hostedImagesPath </> (imageExpectationFilename expectation), [expectation])
  where
    (name, fileType) = nameAndType templateString
    expectation = ImageExpectation { name=name, fileType=fileType, size=QualityHigh }

rewriteImageTemplates :: String -> String -> (String, [ImageExpectation])
rewriteImageTemplates hostedImagesPath source = rewriteTemplates imgTemplateStart defaultExtractTemplateString (imgTemplateRewriter hostedImagesPath) source
