
import Options

main :: IO ()
main = getOptions >>= run >>= putStrLn
