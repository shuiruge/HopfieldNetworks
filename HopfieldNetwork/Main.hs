import State
import Hopfield
import Control.Monad.Writer

learningRate :: LearningRate
learningRate = 1

memory :: [State]
memory = fromBits <$> ["1010101", "010101"]

learningRule :: LearningRule
learningRule = ojaRule

hopfield :: Hopfield
hopfield = foldl (memorize learningRule learningRate) emptyHopfield memory

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
    mapM_ putStrLn $ snd $ runWriter $ iterate' epoch initState