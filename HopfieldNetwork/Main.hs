import State
import Hopfield
import Control.Monad.Writer

getHopfield :: LearningRule -> LearningRate -> [State] -> Hopfield
getHopfield rule rate states = learn' states $ initialize' emptyHopfield
    where state = head states
          indexList = getIndexList state
          initialize' hopfield' = foldr ($) hopfield' (initialize <$> indexList <*> indexList)
          learn' states' hopfield = foldr (learn rule rate) hopfield states'

iterate' :: Hopfield -> Int -> State -> Writer [String] State
iterate' hopfield epochs state
    | epochs == 0 = do
        tell [show state]
        return state
    | otherwise = do
        let newState = ordinalAsynUpdate hopfield state
        tell [show newState]
        iterate' hopfield (epochs - 1) newState

main :: IO ()
main = do
    let rate = 1
        rule = ojaRule 1
        memory = fromBits <$> ["1010101", "0101010"]
        hopfield = getHopfield rule rate memory
        epoch = 5
        initState = fromBits "0000000"
    print hopfield
    mapM_ putStrLn $ snd $ runWriter $ iterate' hopfield epoch initState