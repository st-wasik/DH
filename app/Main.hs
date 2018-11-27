module Main where

import Data.List
import System.Random

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]

main :: IO ()
main = do
    gen1 <- newStdGen
    gen2 <- newStdGen

    let x = head $ randomRs (150,250) gen1
    let n = primes !! x
    putStrLn $ "n = " ++ (show n)

    let g = last $ primitiveRoots n
    putStrLn $ "g = " ++ (show g)

    let _xs@[_x, _y] = map (fromInteger) . take 2 $ randomRs (1000, 10000) gen2
    putStrLn $ "klucze prywatne [x,y]  = " ++ (show _xs)

    let _Xs@[_X, _Y] = map (\x -> fastPow g x n) _xs
    putStrLn $ "klucze publiczne [X,Y] = " ++ (show _Xs)

    let k = [fastPow _Y _x n, fastPow _X _y n]
    putStrLn $ "obliczone klucze sesji = " ++ (show k)

    return ()

primitiveRootModN :: Integer -> Integer -> Bool
primitiveRootModN n r = all (\a -> elem a x1) $ filter (\a -> gcd a n == 1) [1..n-1]
    where x1 = zipWith (\a b -> fastPow a b n) (repeat r) [1..n]

primitiveRoots n = filter (primitiveRootModN n) [2..n]

fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base 1 m = mod base m
fastPow base pow m | even pow = mod ((fastPow base (div pow 2) m) ^ 2) m
                   | odd  pow = mod ((fastPow base (div (pow-1) 2) m) ^ 2 * base) m

                   
quicksort [] = []
quicksort (x:xs) = quicksort lower ++ [x] ++ quicksort greater
    where (lower, greater) = partition (<x) xs 

last' [] = Nothing
last' [x] = Just x
last' (x:xs) = last' xs
