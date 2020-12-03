import Text.Printf

inputPath = "input"

safeTail [] = []
safeTail (x:xs) = xs

treeCounter :: Int -> Int -> [[Int]] -> Int -> Int -> Int
treeCounter _ _ [] res _  = res
treeCounter right down (x:xs) res i = treeCounter right down rest res' i'
    where
        rest = iterate safeTail (x:xs) !! down
        i' = mod (i + right) (length x)
        res' = res + x !! i

first :: [[Int]] -> Int
first input = treeCounter 3 1 input 0 0

second :: [[Int]] -> Int
second input = product . map (\(right, down)-> treeCounter right down input 0 0) $ slopes
    where
        slopes =  [(1, 1), (3,1), (5, 1), (7, 1), (1, 2)]

parse '#' = 1
parse '.' = 0

main :: IO ()
main = do
    input <- map (map parse).lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first input
    printf "Gold star:  \t%d\n" $ second input
