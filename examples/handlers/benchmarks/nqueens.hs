import Control.Monad

safeAddition :: [Int] -> Int -> Int -> Bool
safeAddition [] _ _ = True
safeAddition (r:rows) row i =
   row /= r &&
   abs (row - r) /= i &&
   safeAddition rows row (i + 1)

-- hand-coded solution to the n-queens problem
queensPure :: Int -> [[Int]]
queensPure n = foldM f [] [1..n] where
    f rows _ = [row : rows |
                row <- [1..n],
                safeAddition rows row 1]