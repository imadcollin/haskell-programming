
import Debug.Trace
-- ************************************************
-- Init function in recusive
-- Return the list without the last element
-- ************************************************
myInit ::[a]->[a]
myInit [x] =[]
myInit (x:xs) = x:(myInit xs)


-- ************************************************
-- Function take [1,2,3,4] -> 1234
-- ************************************************
fromDecimals :: [Integer] -> Integer
fromDecimals ns = fromDecimals' ns 0
    where
    -- Variant. length (x:xs)
    fromDecimals' :: [Integer] -> Integer -> Integer
    fromDecimals' [] acc =trace("acc="++ show acc)$ acc
    fromDecimals' (x:xs) acc = fromDecimals' xs  ((acc * 10) + x)     


-- ************************************************
-- Even square in Recursion
-- ************************************************
square1::[Integer]->[Integer]
square1 xs = [x*x| x<-xs,x `mod` 2==0 ] 
--square xs = [x^2| x<-xs,even x  ] 

-- Or
square::[Integer]->[Integer]
square []=[]
square (x:xs) =if (even x) then (x*x):square xs else square xs  
-- Or
squareOfEven :: [Integer] -> [Integer]
squareOfEven [] = []
squareOfEven (n:ns)
--    | odd n = squareOfEven ns --if the elem is odd then ignore and continue with the rest of the list 
    | even n = n *n : squareOfEven ns -- n*n and do untill the base rule is matched == []

-- Or
sq :: [Integer] -> [Integer]
sq [] = []
sq (n:ns)
    | odd n = sq ns --if the elem is odd then ignore and continue with the rest of the list 
    | otherwise = n^2 : sq ns -- n*n and do untill the base rule is matched == []


-- ************************************************
-- MergeSort 
-- ************************************************    

split :: [a] -> ([a],[a]) 
split xs =let l = length xs `div` 2 in (take l xs, drop l xs)
------------
merge :: [Integer] -> [Integer] -> [Integer] 
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | y < x = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)
------------
mergeSort :: [Integer] -> [Integer] 
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let
     (xs1,xs2) = split xs
    in
    merge (mergeSort xs1) (mergeSort xs2)
------------
insert k [] = [k]
insert k (x:xs) =if k < x then k : x : xs else x:(insert k xs)
------------
insertionSortAux sorted [] = sorted 
insertionSortAux sorted (x:xs) =insertionSortAux (insert x sorted) xs
insertionSort xs = insertionSortAux [] xs