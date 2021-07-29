-- Divisor function is multiplicative but not completely multiplicative.
-- Nevertheless, as numbers n and n+1 are always coprime, we can use divisor function approach.  

numberOfDivisors :: Int -> Int
numberOfDivisors n = length [ d | d <- [1 .. n], mod n d == 0]

numberOfDivisorsOfTriangularNumber :: Int -> Int
numberOfDivisorsOfTriangularNumber n
    | even n    = numberOfDivisors (div n 2) * numberOfDivisors (n + 1)
    | otherwise = numberOfDivisors n * numberOfDivisors (div (n + 1) 2)

main :: IO ()
main = (print . (\x -> div (x * (x + 1)) 2) . head . filter (\x -> numberOfDivisorsOfTriangularNumber x > 500)) [1 ..]