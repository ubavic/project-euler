divisibleBy5or3 :: Int -> Bool
divisibleBy5or3 i
    | mod i 3 == 0 = True 
    | mod i 5 == 0 = True
    | otherwise    = False

main :: IO ()
main = print . sum . filter divisibleBy5or3 $ [1 .. 999]