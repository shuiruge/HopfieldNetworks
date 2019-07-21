module ParseMnist
( Input
, Target
, Datum (..)
, readCsv
, parseRecord
, readMnist
) where


import Text.CSV (Record, CSV, parseCSV)
import Text.Parsec.Error (ParseError)

-- Recall the types in 'Text.CSV':
-- type Record = [String]
-- type CSV = [Record]


readCsv :: FilePath -> IO CSV
readCsv path = do
  file <- readFile path
  let
    handleError :: ParseError -> CSV
    handleError = const [[]]  -- type CSV = [[String]]
  -- 'tail' for exlcuding heads
  return $ either handleError tail $ parseCSV path file
 

type Input = [Double]
type Target = Int
data Datum = Datum { input :: Input, target :: Target } deriving Show


toDouble :: String -> Double
toDouble = read


toInt :: String -> Int
toInt = round . toDouble


parseRecord :: Record -> Maybe Datum
parseRecord record =
  if length record /= 785
    then Nothing
  else
    let
      normalize = (/ 255)
      inpt = normalize . toDouble <$> tail record
      targ = toInt $ head record
    in
      Just (Datum inpt targ)


readMnist :: FilePath -> IO [Maybe Datum]
readMnist path = do
  csv <- readCsv path
  return $ parseRecord <$> csv