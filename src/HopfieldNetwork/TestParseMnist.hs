import ParseMnist


main :: IO ()
main = do
  mnist <- readMnist "mnist_test.csv"
  print $ head mnist