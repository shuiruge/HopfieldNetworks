import Index
import Spin
import State
import Hopfield
import ParseMnist
import Control.Monad.Writer


type Step = Int
type Dimension = Int
type InputIds = [Index]
type HiddenIds = [Index]


getInputIds :: Dimension -> InputIds
getInputIds dim = (\i -> Index [0, i]) <$> [1..dim]


getHiddenIds :: Dimension -> HiddenIds
getHiddenIds dim = (\i -> Index [1, i]) <$> [1..dim]


getInitHopfield :: InputIds -> HiddenIds -> IO Hopfield
getInitHopfield inputIds hiddenIds =
  let
    -- Connect hidden indices
    connectHidden :: Hopfield -> Hopfield
    connectHidden h = foldr ($) h (connect <$> hiddenIds <*> hiddenIds)

    -- Connect indices bwtween input and hidden
    connectInputHidden :: Hopfield -> Hopfield
    connectInputHidden h = foldr ($) h (connect <$> inputIds <*> hiddenIds)

    -- Set random weight
    setRandomWeight :: Hopfield -> IO Hopfield
    setRandomWeight = return  -- TODO        

  in
    (setRandomWeight . connectHidden . connectInputHidden) emptyHopfield


getInitState :: InputIds -> HiddenIds -> State
getInitState iids hids = fromList [(i, Down) | i <- iids ++ hids]


relax :: Hopfield -> Step -> HiddenIds -> State -> State
relax hopfield maxStep hiddenIds state =
  if maxStep == 0
    then state
  else let
      state' = asynUpdate hopfield hiddenIds state
      maxStep' =
        if state == state'
          then 0  -- stop iteration.
        else maxStep - 1
    in
      relax hopfield maxStep' hiddenIds state'


setInput :: Datum -> State -> State
setInput d s =
  let
    toSpin :: Double -> Spin
    toSpin x | x > 0.3 = Up
              | otherwise = Down

    enum :: [a] -> [(Int, a)]
    enum = zip [1..]

    update :: (Index, Spin) -> State -> State
    update (i, sp) = updateState i sp
  in
    foldr update s [(Index [0, i], toSpin x) | (i, x) <- enum (input d)]
  

iterate_ :: Step  -- max step in relaxing
         -> LearningRule
         -> LearningRate
         -> HiddenIds
         -> Datum
         -> (Hopfield, State)  -- to iterate
         -> (Hopfield, State) 
iterate_ n rule lr hids d (h, s) =
  let
    s' = relax h n hids (setInput d s)
    h' = learn rule lr s' h
  in
    (h', s')


main :: IO ()
main = do
  let
    maxStep = 5
    rule = ojaRule 1
    lr = 0.1
    inputDim = 784
    hiddenDim = 8
    inputIds = getInputIds inputDim
    hiddenIds = getHiddenIds hiddenDim
    state = getInitState inputIds hiddenIds

    train :: [Maybe Datum]
          -> (Hopfield, State)
          -> Writer [String] (Hopfield, State)
    train [] (h, s) = do
      tell [show h]
      return (h, s)
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
    train (take 10 mnist) (hopfield, state)