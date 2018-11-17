
--Using Types and if else statments  

-- u:: (String , String , String , Int, String)-> Int -> String-> ()

u:: (String , String , String , Int, String)-> Int -> String-> (String , String , String , Int, String)
u  (f,g,c,d,e) x str =if x == 1 then (f,str,c,d,e) 
                 else if x==2 then (str,g,c,d,e) else  ("Please", "make sure ","the INT is" ,1,"or 2")

-- Uising Guards 
u2  (f,g,c,d,e) x str 
    | x == 1 =  (f,str,c,d,e) 
    | x ==2 = (str,g,c,d,e)
    | otherwise = ("Please", "make sure ","the int is" ,1,"or 2")

-- Case Experssion 
u3 (f,g,c,d,e) x str = 
    case x of 
        1-> (f,str,c,d,e) 
        2-> (str,g,c,d,e)
        _-> ("Please", "make sure ","the int is" ,1,"or 2")

-- Many other ways to do that..... 
updatePersonName (fn,_,sa,pn,pa) 1 gn = (fn,gn,sa,pn,pa)
updatePersonName (_,gn,sa,pn,pa) 2 fn = (fn,gn,sa,pn,pa)