-- Contains Problems 1 to 20

-- Problem 1

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (x:xs) = myLast xs


-- Problem 2

myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (x:xs) | length(xs) == 1 = Just x
                 | otherwise = myButLast xs


-- Problem 3

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) n | n > length(x:xs) = Nothing
                   | n > 1 = elementAt (xs) $ n-1
                   | otherwise = Just x

-- Problem 4

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6

isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome (x:xs) = myReverse (x:xs) == (x:xs)

-- Problem 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) =  flatten x ++ flatten (List xs)

-- Problem 8

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) | xs == [] = [x]
                | x == (xs!!0) = compress xs
                | otherwise = x : compress xs

-- Simpler one liner : compress' (x:xs) = x: ( compress $dropWhile (==x) xs)

-- Problem 9

pack ::(Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:(takeWhile (==x) xs)) : (pack $ dropWhile (==x) xs)

-- Problem 10

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = let 
                  packed = pack (x:xs)
                in
                  encodeHelper packed
          where
            encodeHelper [] = []
            encodeHelper (y:ys) = (length(y),y!!0) : encodeHelper ys

-- Problem 11

data Occurance a = Multiple Int a | Single a


instance (Show a) => Show (Occurance a) where
      show (Multiple n x) = "Multiple " ++ show n ++ " " ++ show x
      show (Single x) = "Single " ++ show x

encodeModified :: (Eq a) => [a] -> [Occurance a]
encodeModified [] = []
encodeModified (x:xs) = let 
                  packed = pack (x:xs)
                in
                  encodeHelper packed
          where
            encodeHelper [] = []
            encodeHelper (y:ys) | length(y) > 1 = (Multiple (length y) (y!!0)) : encodeHelper ys
                                | otherwise = (Single (y!!0)) : encodeHelper ys

-- Slightly different variation using map (encodeHelper.encode) (x:xs)

-- Problem 12

decodeModified :: [Occurance a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
                          (Single b) -> b : decodeModified xs
                          (Multiple n b) -> (replicate n b) ++ decodeModified xs
                          
 
-- Problem 13

encodeDirect ::(Eq a) => [a] -> [(Int, a)]
encodeDirect (x:xs) = foldr directHelper [] (x:xs)
                where
                      directHelper x [] = [(1,x)] 
                      directHelper x ((a,b):ys) | x == b = (1+a,x):ys
                                                | otherwise = (1,x):(a,b):ys

-- Problem 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

-- Probelm 15

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = repliHelper x n ++ repli xs n 
              where
               repliHelper _ 0 = []
               repliHelper y n = y :  repliHelper y  (n-1)

-- Problem 16 

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery (x:xs) n | n <= length(x:xs) = take (n-1) (x:xs) ++ dropEvery (drop n (x:xs)) n 
                   | otherwise = (x:xs) 



-- Problem 17 (No predefined predicates)

split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split (x:xs) n | n > 0 =  mergeTup ([x],[]) $ split xs $ n-1
               | otherwise = ([],(x:xs))
              where
                mergeTup (a,b) (c,d) = (a++c,b++d)


-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i k = take (k-i+1) . drop (i-1) $ xs

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n | n > 0 = drop n xs ++ take n xs
            | n < 0 = reverse $ rotate (reverse xs) (-1 * n)
            | otherwise = xs

-- Problem 20

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing,[]) 
removeAt n (x:xs) = (Just (head b2), b1 ++ tail b2)  
                  where
                    (b1,b2) = split (x:xs) (n-1)


-- Problem 21

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt x xs n = l1 ++ [x] ++ l2
                where
                  (l1,l2) = split' xs  $n-1


-- Problem 22

range:: Int -> Int -> [Int]
range i j | i <= j = i: range (i+1) j
          | otherwise = []

-- Problem 23



