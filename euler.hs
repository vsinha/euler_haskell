import Data.List (union, nub)

problem_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

problem_2 = sum [x | x <- takeWhile (<= 4000000) fibs, even x]
    where
        fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


-- A number is prime if it is even & has no prime factors
primes = 2 : filter (null . tail . primeFactors) [3, 5..]

primeFactors n = factor n primes
    where 
        factor n (p:ps)
            | p * p > n         = [n]
            | n `mod` p == 0    = p : factor (n `div` p) (p:ps)
            | otherwise         = factor n ps

problem_3 = last $ primeFactors 600851475143


listProduct :: (Num a, Eq a) => [a] -> [a]
listProduct xs = [x * y | x <- xs, y <- xs]

digitsRev :: Integral n
       => n   -- Base to use (ie 10)
       -> n   -- The number to convert into digit form
       -> [n] -- The digits of the number as a list
digitsRev base i = case i of
                     0 -> []
                     _ -> lastDigit : digitsRev base rest
                       where (rest, lastDigit) = quotRem i base

isPalindrome :: Integral n => n -> Bool
isPalindrome n = 
    let list = digitsRev 10 n
     in list == reverse list

problem_4 = maximum $ filter isPalindrome $ listProduct [100..999]

-- way shorter (from forums)
problem_4' = maximum [x | y <-[100..999], z <-[y..999], 
    let x = y*z, 
    let s = show x, 
        s == reverse s]

problem_5 = do
    -- list of list of factors of each number from 1 to 20
    let factors = map primeFactors [1..20]

    let count x = length . filter (x==)

    -- value v at index i is the max number of occurences of i as a factor
    let factorCounts = (map (\i -> maximum (map (count i) factors)) [1..20])

    -- multiply all i^v together to get the least common multiple 
    foldl (\a (x, y) -> a * x^y) 1 (zip [1..20] factorCounts)



