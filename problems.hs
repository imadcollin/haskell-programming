-- ************************************************
-- (*) Find the last element of a list.
-- ************************************************
findLast []= error "Empty list! "
findLast  [x]=x
findLast (x:xs)= if length (xs)== 0 then x else findLast xs 
-- Or
fl []=error "empty list"
fl [x]=x 
fl (_:xs)= fl xs

-- ************************************************
-- (*) Find the last but one element of a list.
-- ************************************************
nl []= error "Empty list" 
nl [x]= error "One elem"
nl [x,_]=x 
nl (x:xs)=nl xs

-- ************************************************
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- ************************************************
kth ::[a]->Int -> a 
kth [] _= error "empty list!"
kth _ 0 =error "Start for 1 "
kth (x:xs) n =  head (drop (n-1) (x:xs) ) --[1,2,3,4]  3 -> 3 

-- ************************************************
-- (*) Find the number of elements of a list.
-- ************************************************
count:: [a]->Int
count []= 0 
count [x]=1
count (x:xs)=1+ count xs

-- Or

countAcc::[a]->Int
countAcc list = countAcc' list 0 
    where 
        countAcc' [] acc = acc
        countAcc' (x:xs) acc = countAcc' xs (acc+1)  

-- ************************************************
-- (*) Reverse a list.
-- ************************************************
rev::[a]->[a]
rev []= []
--rev [x]=[x]
rev (x:xs) = (rev xs)++[x]


-- Or 
revAcc:: [a]->[a]
revAcc list = revAcc' list [] 
        where 
            revAcc' [] acc = acc
            revAcc' (x:xs) acc = revAcc' xs (x:acc)

-- ************************************************
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; 
--     e.g. (x a m a x).
-- ************************************************
pal:: [a]->Bool 
pal [] = True
--pal xs= (head xs) == (last xs) -- &&(pal $ init $ tail xs)
-- To be continued 