import Ising
import Metropolis

main :: IO()
main = do
    let state = getState [-1, 1, 1]
    putStrLn . show $ state
