isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = 0 `notElem` map (mod n) [2 .. n-1]

main :: IO ()
main = print $ product [p^e | p <- primes, e <- [1..4], p^e < 20, p^(e+1) > 20]
    where primes = filter isPrime [1 .. 20]