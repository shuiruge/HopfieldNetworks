import Index
import State
import Hopfield
import Control.Monad.Writer

getHopfield :: LearningRule -> LearningRate -> [State] -> Hopfield
getHopfield rule rate states =
  let
    state = head states
    indexList = Zero : getIndexList state
    initialize' hopfield' = foldr ($) hopfield'
                                  (connect <$> indexList <*> indexList)
    learn' = foldr (learn rule rate)
  in
    learn' (initialize' emptyHopfield) states

iterate' :: Hopfield -> Int -> State -> Writer [String] State
iterate' hopfield epochs state
  | epochs == 0 = return state
  | otherwise = do
    let
      e = energy hopfield state
      state' = ordinalAsynUpdate hopfield state
      e' = energy hopfield state'
      epochs'
        | e == e' = 0  -- stop iteration.
        | otherwise = epochs - 1
    tell ["State  | " ++ show state]
    tell ["Energy | " ++ show e]
    tell ["------ | --------"]
    iterate' hopfield epochs' state'

main :: IO ()
main = do
  let
    rate = 1
    -- rule = ojaRule 1
    rule = hebbRule
    memory = fromBits
          <$> [ "1010101010101"
              , "0101010101010"
              , "1001001001001" ]
    hopfield = getHopfield rule rate memory
    epoch = 5
    initState = fromBits "1001001001101"
    write = mapM_ putStrLn . snd . runWriter
  write $ iterate' hopfield epoch initState