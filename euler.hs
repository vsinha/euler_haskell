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


digitsRev :: Integral n
       => n   -- Base to use (ie 10)
       -> n   -- The number to convert into digit form
       -> [n] -- The digits of the number as a list
digitsRev base i = case i of
                     0 -> []
                     _ -> lastDigit : digitsRev base rest
                       where (rest, lastDigit) = quotRem i base
                             
digits base = reverse . digitsRev base

isPalindrome :: Integral n => n -> Bool
isPalindrome n = 
    let list = digitsRev 10 n
     in list == reverse list

problem_4 = maximum $ filter isPalindrome $ listProduct [100..999]
  where listProduct xs = [x * y | x <- xs, y <- xs]

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


sumOfSquares xs = sum (map (\x -> x^2) xs)

squareOfSums xs = (sum xs) ^ 2

-- sum square difference
problem_6 = (squareOfSums [1..100]) - (sumOfSquares [1..100])

-- 10,001th prime
problem_7 = last $ take 10001 primes

thousandDigitNumber = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

thirteenLists :: [a] -> [[a]]
thirteenLists [] = []
thirteenLists xs = [take 13 xs] ++ thirteenLists (tail xs)

listProduct xs = foldl (*) 1 xs

problem_8 = maximum $ map listProduct listsWithoutZero
  where listProduct xs = foldl (*) 1 xs
        listsWithoutZero = filter (not . elem 0) $ 
          thirteenLists $ digits 10 thousandDigitNumber




