-- ************************************************
-- Length of List 
-- ************************************************
l_len::[a] -> Integer
l_len []= 0
l_len (x:xs) = 1+ l_len xs  

-- ************************************************
-- Sum of the list
-- Sum willl produce Ambiguous occurrence. 
-- ************************************************
l_sum [] = 0
l_sum (x:xs)= x+ l_sum xs 

-- ************************************************
--Factorial 
-- ************************************************
fact::Integer-> Integer
fact 0 = 0
fact 1 =1 
fact n = n*fact (n-1)   

--fac 0= 1 
--fac x = x* fac(x-1)

-- ************************************************
-- Revers a list 
-- ************************************************
l_rev:: [a]-> [a]
l_rev []= []
l_rev (x:xs)= (l_rev xs++ [x]) 


-- ************************************************
-- Repeat x  ntime  
-- ************************************************
rep _ 0 = []
rep x n = x:rep x (n-1) 

--rep x 0= []
--rep x n= x:rep (n-1) n
--rep (x:xs) n  =  x

-- ************************************************
-- Count element in list  
-- ************************************************
cout [] = 0
cout [x]=1 
cout (x:xs)=1+ cout xs 

-- ************************
-- Compute gcd  
-- ************************
gcd_re x y = if y == 0 then x else gcd_re y (x `mod` y)  

-- OR 
gcd2 x 0 = x 
gcd2 x y = gcd2 y (x `mod` y)

-- ************************************************
-- Power, Exponentiation 
-- ************************************************
pow x 0 = 1 
pow x n = x* pow x (n-1)

-- ************************************************
-- Mak two numbes 
-- ************************************************
max1 [] =error "empty list !!"
max1 [x]= x
max1 (x:xs) = if x > larg then x  else larg where larg = max1 xs 

-- ************************************************
-- Fibonancci Numbers   
-- ************************************************
fib 0= 0 
fib 1 = 1
fib n = fib (n-1) +fib (n-2) 

-- ************************************************
-- Towers of Hanoi 
-- "BUG:" Not Working yet 
-- ************************************************
han 0 _ _ _ = [] 
hanoi n src dest aux = hanoi (n - 1) src aux dest ++ [(src,dest)] ++ hanoi (n - 1) aux dest src

-- ************************************************
-- Sum a to b 
-- ************************************************
sum_int 0 0 = 0 
sum_int a b = do if b < a then 0  else a+ sum_int (a+1) b 

--Better way 
s 0 0 = 0 
s x 0 = 0 
s x y = if x < y then x + s (x+1) y  else 0 

-- ************************************************
-- Integer to String 
-- ************************************************
int2str 1 = "1" 
int2str 2 = "2" 
int2str 3 = "3" 
int2str 4 = "4" 
int2str 5 = "5" 
int2str 6 = "6" 
int2str 7 = "7" 
int2str 8 = "8" 
int2str 9 = "9" 
int2str n = if n > 9 then int2str (n `div` 10) ++ int2str (n `mod` 10)   else "-"++ int2str (abs n)

-- ************************************************
-- Integer to String  to be continued.... 
-- ************************************************
int2str2 n =if n >(-1) && n < 9 then t!!(n-1) else "empt" where t=["1","2","3","4","5"]     

-- ************************************************
-- String properties .... 
-- ************************************************
se "_" s = -1
se "" "" = error "emoty list !!" 
se s s' = if take (length s') s == s' then 0 else case se (tail s ) s' of 
    (-1) -> (-1)
    n -> n+1  

-- ************************************************
-- Replicate
-- ************************************************
repl 0 _=[]
repl n x = x:(repl (n-1) x) 
-- ************************************************
-- From to 
-- ************************************************
fromto x y= if x > y then [] 
            else if x==y then [y]
            else x:(fromto (x+1) y)

