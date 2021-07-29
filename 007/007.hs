isPrime' :: Int -> Bool
isPrime' n
    | n <= 1 = False
    | otherwise = 0 `notElem` map (mod n) [3 .. n-1]

main :: IO ()
main = print $ filter isPrime' [3 ..] !! 10000