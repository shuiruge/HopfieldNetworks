import Index
import State
import Hopfield
import Control.Monad.Writer
import Util

getHopfield :: LearningRule -> LearningRate -> Int -> [State] -> Writer [String] Hopfield
getHopfield rule rate epochs states =
  let
    state = head states
    indexList = getIndexList state
    initialize' hopfield' = foldr ($) hopfield'
                                  (connect <$> indexList <*> indexList)
    
    learn' :: Hopfield -> [State] -> Writer [String] Hopfield
    learn' hopfield (st:sts) = do
      let
        norm = weightNorm 2 hopfield
        newHopfield = learn rule rate st hopfield
      tell ["Norm of weight: " ++ show norm]
      if sts == []
        then return $ newHopfield
      else
          learn' newHopfield sts
  in
    learn' (initialize' emptyHopfield) (duplicate epochs states)

ordinalAsynUpdate :: Hopfield -> State -> State
ordinalAsynUpdate hopfield state = asynUpdate hopfield (getIndexList state) state

iterate' :: Hopfield -> Int -> State -> Writer [String] State
iterate' hopfield maxStep state
  | maxStep == 0 = return state
  | otherwise = do
    let
      e = energy hopfield state
      state' = ordinalAsynUpdate hopfield state
      e' = energy hopfield state'
      maxStep'
        | e == e' = 0  -- stop iteration.
        | otherwise = maxStep - 1
    tell ["State  | " ++ show state]
    tell ["Energy | " ++ show e]
    tell ["------ | --------"]
    iterate' hopfield maxStep' state'

main :: IO ()
main = do
  let
    rate = 0.1
    -- rule = hebbRule
    rule = ojaRule 1
    memory = fromBits
          <$> [ "1010101010101"
              , "0101010101010"
              , "1001001001001" ]
    epochs = 10
    (hopfield, learnLog) = runWriter $ getHopfield rule rate epochs memory
    maxStep = 5
    initState = fromBits "1001001011111"
  putStrLn "\nLearning Process......\n"
  mapM_ putStrLn learnLog
  putStrLn "\nUpdating Process......\n"
  mapM_ putStrLn . snd . runWriter $ iterate' hopfield maxStep initState