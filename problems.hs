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
