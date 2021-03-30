module Main where
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.List (groupBy)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

ask :: IO String
ask = putStr "code: " >> hFlush stdout >> getLine

loop :: Map String String -> IO ()
loop m = do
    guess <- ask
    case m !? guess of
        Just reward -> putStrLn reward >> loop m
        Nothing -> putStrLn "Wrong guess" >> loop m
    
mappify :: [String] -> Map String String
mappify = M.fromList . map split
    where split str = (takeWhile (/= ':') str, tail . dropWhile (/= ':') $ str)

splitOnSemicolon :: String -> [String]
splitOnSemicolon = map removeLeading . filter (/= ";") . groupBy semiEq
    where semiEq _ ';' = False
          semiEq ';' _ = False
          semiEq _ _ = True
          removeLeading [] = []
          removeLeading ('\n':rest) = removeLeading rest
          removeLeading (x:rest) = x:rest

main :: IO ()
main = do
    clearScreen
    args <- getArgs
    case args of
        [] -> let m = mappify . splitOnSemicolon <$> readFile "coderewards.txt" in m >>= loop
        _ -> let m = mappify args in loop m 