import Index
import Spin
import State
import Hopfield
import ParseMnist
import Util (foldr', foldM')
import Control.Monad.Writer
import System.Random


type Step = Int
type Dimension = Int
type InputIds = [Index]
type HiddenIds = [Index]


getInputIds :: Dimension -> InputIds
getInputIds dim = (\i -> index [0, i]) <$> [1..dim]


getHiddenIds :: Dimension -> HiddenIds
getHiddenIds dim = (\i -> index [1, i]) <$> [1..dim]


getInitHopfield :: InputIds -> HiddenIds -> IO Hopfield
getInitHopfield iids hids =
  let
    std = sqrt . (1 /) $ fromIntegral (length iids + length hids)

    connect' :: Hopfield -> (Index, Index) -> IO Hopfield
    connect' h (i, j) = do
      rand <- randomIO :: IO Double  -- in range [0, 1)
      let w = std * (2 * rand - 1)
      return $ connectWith w i j h

    mix :: [a] -> [b] -> [(a, b)]
    mix [] _ = []
    mix (x:xs) ys = [(x, y) | y <- ys] ++ (mix xs ys)

    indexPairs = mix hids hids ++ mix iids hids
  in
    foldM' connect' emptyHopfield indexPairs


getInitState :: InputIds -> HiddenIds -> IO State
getInitState iids hids = do
  let
    readSpin :: Double -> Spin
    readSpin x
      | x >= 0.5 = Up
      | otherwise = Down

    indexRandomSpin:: Index -> IO (Index, Spin)
    indexRandomSpin i = do
      rand <- randomIO :: IO Double  -- in range [0, 1)
      return (i, readSpin rand)
  indexSpinList <- mapM indexRandomSpin (iids ++ hids)
  return $ state indexSpinList
    


relax :: Hopfield -> Step -> HiddenIds -> State -> State
relax h maxStep hids s =
  if maxStep == 0
    then s
  else let
      s' = asynUpdate h hids s
      maxStep' =
        if s == s'
          then 0  -- stop iteration.
        else maxStep - 1
    in
      relax h maxStep' hids s'


setInput :: Datum -> State -> State
setInput d s =
  let
    toSpin :: Double -> Spin
    toSpin x | x > 0.1 = Up
             | otherwise = Down

    enum :: [a] -> [(Int, a)]
    enum = zip [1..]

    update :: (Index, Spin) -> State -> State
    update (i, sp) = updateState i sp
    idxSpinList = [(index [0, i], toSpin x) | (i, x) <- enum (input d)]
  in
    foldr' update s idxSpinList
  

iterate_ :: Step  -- max step in relaxing
         -> LearningRule
         -> LearningRate
         -> HiddenIds
         -> Datum
         -> (Hopfield, State)  -- to iterate
         -> (Hopfield, State) 
iterate_ n rule lr hids d (h, s) =
  let
    sd = setInput d s
    s' = relax h n hids sd
    h' = learn rule lr s' h
  in
    (h', s')


main :: IO ()
main = do
  let
    maxStep = 5
    -- rule = hebbRule
    rule = ojaRule 1
    lr = 0.1
    inputDim = 784
    hiddenDim = 1024
    inputIds = getInputIds inputDim
    hiddenIds = getHiddenIds hiddenDim

    train :: [Maybe Datum]
          -> (Hopfield, State)
          -> Writer [String] (Hopfield, State)
    train [] (h, s) = return (h, s)
    train (Nothing : ds) (h, s) = train ds (h, s)
    train (Just datum : ds) (h, s) = do
      let
        (h', s') = iterate_ maxStep rule lr hiddenIds datum (h, s)
        eng = energy h' s'
        norm = weightNorm 2 h'
      tell ["H: " ++ show eng]
      tell ["|W|: " ++ show norm]
      train ds (h', s')

  state <- getInitState inputIds hiddenIds
  hopfield <- getInitHopfield inputIds hiddenIds
  mnist <- readMnist "mnist_test.csv"
  mapM_ putStrLn . snd . runWriter $
    train (take 2 mnist) (hopfield, state)