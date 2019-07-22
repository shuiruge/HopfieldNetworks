import Index
import Spin
import State
import Hopfield
import ParseMnist
import Util (foldr', foldM')
import Control.Monad.Writer
import Data.Random
import Debug.Trace


type Step = Int
type Dimension = Int
type InputIds = [Index]
type HiddenIds = [Index]


getInputIds :: Dimension -> InputIds
getInputIds dim = (\i -> Index [0, i]) <$> [1..dim]


getHiddenIds :: Dimension -> HiddenIds
getHiddenIds dim = (\i -> Index [1, i]) <$> [1..dim]


getInitHopfield :: InputIds -> HiddenIds -> IO Hopfield
getInitHopfield iids hids =
  let
    std = sqrt . (1 /) $ fromIntegral (length iids + length hids)

    connect' :: Hopfield -> (Index, Index) -> IO Hopfield
    connect' h (i, j) = do
      rand <- runRVar (normal 0 1) StdRandom :: IO Double
      let w = std * rand
      return $ connectWith w i j h

    mix :: [a] -> [b] -> [(a, b)]
    mix [] _ = []
    mix (x:xs) ys = [(x, y) | y <- ys] ++ (mix xs ys)

    indexPairs = mix hids hids ++ mix iids hids
  in
    foldM' connect' emptyHopfield indexPairs


getInitState :: InputIds -> HiddenIds -> State
getInitState iids hids = fromList [(i, Down) | i <- iids ++ hids]


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
      trace ("---" ++ show s') $ relax h maxStep' hids s'


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
    idxSpinList = [(Index [0, i], toSpin x) | (i, x) <- enum (input d)]
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
    sd1 = trace ("Set input: " ++ show sd) sd
    s' = relax h n hids sd1
    s'1 = trace ("Relaxed : " ++ show s') s'
    h' = learn rule lr s'1 h
  in
    (h', s')


main :: IO ()
main = do
  let
    maxStep = 5
    rule = ojaRule 1
    lr = 0.1
    inputDim = 784
    hiddenDim = 128
    inputIds = getInputIds inputDim
    hiddenIds = getHiddenIds hiddenDim
    state = getInitState inputIds hiddenIds

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

  hopfield <- getInitHopfield inputIds hiddenIds
  mnist <- readMnist "mnist_test.csv"
  mapM_ putStrLn . snd . runWriter $
    train (take 3 mnist) (hopfield, state)