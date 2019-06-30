import Hopfield
import Util

learningRate :: Double
learningRate = 0.1

memory :: [State]
memory = fromBits <$> ["101010", "010101"]

hopfield :: Hopfield
hopfield = foldl (memorize learningRate) emptyHopfield memory

iter :: State -> [Index] -> State
iter = foldl (update hopfield)

iterByRandomIndices :: State -> IO State
iterByRandomIndices state = do
    randomIndices <- shuffle [1..(len state)]
    return $ iter state randomIndices

repeat' :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeat' n f x
    | n == 1 = f x
    | otherwise = do
        y <- f x
        repeat' (n - 1) f y

main :: IO ()
main = do
    print hopfield
    let maxIterStep = 20
        initState = fromBits "101010"
    finalState <- repeat' maxIterStep iterByRandomIndices initState
    print finalState
    
