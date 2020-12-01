import Data.Set (member, fromList)

inputPath = "input"    
match = (2020 -)

first :: [Int] -> Int
first input = head [x * match x | x <- input, member (match x) set]
    where
        set = fromList input

second :: [Int] -> Int
second input = head [x * y * match (x + y) | x <- input, y <- input, member (match (x + y)) set]
    where
        set = fromList input

main :: IO ()
main = do
    str <- readFile inputPath
    let input = map read . lines $ str
    putStrLn $ "Silver star: " ++ show (first input)
    putStrLn $ "Gold star: " ++ show (second input)
