import Control.Exception
import System.Environment
import Data.List
import Data.Array
list1 = ["first","sekond","third"]
printListIndex :: IO()
printListIndex = do catch (print(list1 !! 3)) handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Caught exception: " ++ show ex


main :: IO()
main = do
    result <- try (evaluate (5 `div` 0)) :: IO (Either ArithException Int)
    case result of
        Left ex  -> putStrLn $ "Caught exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val
    printListIndex