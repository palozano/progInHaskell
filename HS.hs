-- quick sort implementation
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]


--seqn :: [IO a] -> IO [a]
seqn :: Monad m => [m a] -> m [a]
seq [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)


-- Ex 1.3
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs


-- Ex. 2.3
n :: Int
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]


-- Ex. 3.2
bools' :: [Bool]
bools' = [True, False, True]

nums' :: [[Int]]
nums' = [[1,2,3],[4,5,6]]

add' :: Int -> Int -> Int -> Int -> Int
add' x y z w = x + y + z + w

copy' ::  a -> (a,a)
copy' x = (x,x)

apply' :: (a -> b) -> a -> b
apply' f x = f x


-- Ex. 3.3
second' :: [a] -> a
second' xs = head (tail xs)

swap' :: (a,b) -> (b,a)
swap' (x,y) = (y,x)

pair' :: a -> b -> (a,b)
pair' x y = (x,y)

double' :: Num x => x -> x
double' x = 2 * x

palindrome' :: Eq a => [a] -> Bool
palindrome' xs = reverse xs == xs

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

-- Ex. 4.1
halve' :: [a] -> ([a],[a])
halve' xs = (take half xs, drop half xs)
            where half = length xs `div` 2

-- Ex. 4.2
third' :: [a] -> a
third' xs = head (tail (tail xs))

third2' :: [a] -> a
third2' xs = xs !! 2

third3' :: [a] -> a
third3' (_:_:x:_) = x

-- Ex. 4.3
-- conditional expresion
safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

-- guarded equations
safetail2' :: [a] -> [a]
safetail2' xs | null xs = []
              | otherwise = tail xs

-- pattern matching
safetail3' :: [a] -> [a]
safetail3' [] = []
safetail3' xs = tail xs

-- Ex. 4.4
--(||) :: Bool a => a -> a -> a
--False || False = False
--_ || _ = True
--
--False || b = b
--True || _ = True
--
--False || False = False
--False || True = True
--True || False = True
--True || True = True
--
--b || c | b == c = b
--       | otherwise = True

-- Ex. 4.7
mult' :: Int -> Int -> Int -> Int
mult' = \x -> (\y -> (\z -> x*y*z))

-- Ex. 4.8
luhnDouble :: Int -> Int
luhnDouble x = if x*2 > 9 then x*2-9 else x*2

luhnDouble' :: Int -> Int
luhnDouble' x | x * 2 > 9 = (x * 2) - 9
             | otherwise = x * 2

-- second part
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0
