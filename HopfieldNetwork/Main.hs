import State
import Hopfield
import Control.Monad.Writer

learningRate :: LearningRate
learningRate = 1

learningRule :: LearningRule
learningRule = ojaRule 1

memory :: [State]
memory = fromBits <$> ["1010101", "0101010"]

hopfield :: Hopfield
hopfield = foldr (learn learningRule learningRate) initHopfield memory
    where state = head memory
          indexList = getIndexList state
          initHopfield = foldr ($) emptyHopfield (initialize <$> indexList <*> indexList)

iterate' :: Int -> State -> Writer [String] State
iterate' epochs state
    | epochs == 0 = do
        tell [show state]
        return state
    | otherwise = do
        let newState = ordinalAsynUpdate hopfield state
        tell [show newState]
        iterate' (epochs - 1) newState

main :: IO ()
main = do
    let epoch = 5
        initState = fromBits "0000000"
    print hopfield
    mapM_ putStrLn $ snd $ runWriter $ iterate' epoch initState