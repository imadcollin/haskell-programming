-- ************************************************
-- Curried Functions
-- ************************************************
mult :: (Num a) => a -> a -> a 
mult x y  = x * y 

--Function 2 
mult2 = mult 3 


--Another example: 
add:: (Num a ) => a ->a ->a ->a 
add a b c = a*b+c 
--Function  2 
add2  = add 5


--Another examples 
divide::(Floating a)=> a ->a 
divide =(/10)
addOne= (+1)
multByFive=(*5)

isLower:: Char ->Bool 
isLower= (`elem` ['a'..'z'])

-- ************************************************
-- higher-orderism is in order
-- ************************************************

addTwice:: (a->a )->a ->a 
addTwice f x = f (f x) --apply f twice then do the x operation 
-- Usage example addTwice (+5) 3 = 5+5 + 3    

-- ************************************************
-- Map 
-- ************************************************
maper:: (a->b)->[a]->[b] 
maper _ [] = []
maper f (x:xs)= f x: maper f xs 

-- ************************************************
-- Filter
-- ************************************************
filtering:: (a->Bool)->[a]->[a]
filtering _ []=[]
filtering p (x:xs) 
    | p x =x :filtering p xs 
    | otherwise =filtering p xs  