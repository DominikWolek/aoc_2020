import Text.Printf
import qualified Par1
import qualified Par2
import Abs

inputPath = "input"

calc (ExpLit x) = x
calc (ExpMul x y) = calc x * calc y
calc (ExpAdd x y) = calc x + calc y

solution :: [Stmt] -> Integer
solution = sum.map (\(SExp x) -> calc x)

main :: IO ()
main = do
    input <- readFile inputPath
    let (Right (Input input1)) = Par1.pIn $ Par1.myLexer input
    let (Right (Input input2)) = Par2.pIn $ Par2.myLexer input

    printf "Silver star:\t%d\n" $ solution input1
    printf "Gold star:  \t%d\n" $ solution input2