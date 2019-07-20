module ParseMnist
( Input
, Target
, Datum(..)
, readMnist
) where


import Text.CSV (Record, CSV, parseCSV)
import Text.Parsec.Error (ParseError)

-- Recall the types in 'Text.CSV':
-- type Record = [String]
-- type CSV = [Record]


getCsv :: FilePath -> IO CSV
getCsv path = do
  file <- readFile path
  let
    handleError :: ParseError -> CSV
    handleError = const [[]]  -- type CSV = [[String]]
  -- 'tail' for exlcuding heads
  return $ either handleError tail $ parseCSV path file
 

type Input = [Float]
type Target = Int
data Datum = Datum { input :: Input, target :: Target } deriving Show


toFloat :: String -> Float
toFloat = read


toInt :: String -> Int
toInt = round . toFloat


parseRecord :: Record -> Maybe Datum
parseRecord record =
  if length record /= 785
    then Nothing
  else
    let
      normalize = (/ 255)
      input = normalize . toFloat <$> tail record
      target = toInt $ head record
    in
      Just (Datum input target)


readMnist :: FilePath -> IO [Maybe Datum]
readMnist path = do
  csv <- getCsv path
  return $ parseRecord <$> csv