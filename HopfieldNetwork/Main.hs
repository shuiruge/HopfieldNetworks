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
    | epochs == 0 = do
        tell [show state]
        return state
    | otherwise = do
        let
            newState = ordinalAsynUpdate hopfield state
        tell [show newState]
        tell [show $ energy hopfield newState]
        iterate' hopfield (epochs - 1) newState

main :: IO ()
main = do
    let
        rate = 1
        rule = ojaRule 1
        memory = fromBits <$> ["1010101", "0101010"]
        hopfield = getHopfield rule rate memory
        epoch = 5
        initState = fromBits "0000000"
        write = mapM_ putStrLn . snd . runWriter
    print memory
    print hopfield
    write $ iterate' hopfield epoch initState