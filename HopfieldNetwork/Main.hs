import Index
import State
import Hopfield
import Control.Monad.Writer
import Util

getHopfield :: LearningRule -> LearningRate -> Int -> [State] -> Hopfield
getHopfield rule rate epochs states =
  let
    state = head states
    indexList = getIndexList state
    initialize' hopfield' = foldr ($) hopfield'
                                  (connect <$> indexList <*> indexList)
    learn' = foldr (learn rule rate)
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
    epochs = 1
    hopfield = getHopfield rule rate epochs memory
    maxStep = 5
    initState = fromBits "1001001011111"
    write = mapM_ putStrLn . snd . runWriter
  print hopfield
  write $ iterate' hopfield maxStep initState