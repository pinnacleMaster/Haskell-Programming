import Control.Applicative -- <*> Applicative class
data   ME = Num Int 
          | Var Char
          | Group ME
          | Sum [ME]
          | Product [ME] 
          | Power ME Int
          | Neg ME
          deriving (Show, Ord, Eq)


-- Main Program
simplifyME :: ME -> ME
simplifyME e = e

-- Unparser
unparseME :: Maybe ME -> [Char]
unparseME (Just (Num n)) = show n
unparseME (Just (Var v)) = [v]
unparseME (Just (Neg n)) = ['-']++unparseME (Just n)
unparseME (Just (Power f e)) = unparseME(Just f)++"**"++(show e)
unparseME (Just (Sum s))
   |   tail s /= []  =  unparseME(Just (head s))++"+"++unparseME(Just (Sum (tail s))) 
   |   otherwise     =  unparseME(Just (head s))
--unparseME (Just (Product p)) 


-- Derivative`
derivative:: Char -> Maybe ME -> Maybe ME
derivative var (Just (Num n)) = Just(Num 0)
derivative var (Just (Var v))
    | v == var   = Just(Num 1)
    | otherwise  = Just(Num 0)
derivative var (Just (Neg n)) = Just(Neg neg) where Just neg = derivative var (Just n)  
derivative var (Just (Sum s)) = Just(Sum der_s) where der_s  = get_sum_der var s  
derivative var (Just (Product p)) = Just(Sum der_p) where der_p = get_prod_der var p
derivative var (Just (Power (Var x) n))  
     | x == var && ((n-1)>1 || (n-1) < 1)  = Just (Product [(Power (Var x) (n-1)), (Num n)])
     | x == var && ((n-1) == 1)        = Just (Product[(Var x), (Num 2)]) 
     | otherwise = Just (Num 0) 
--derivative var (Just (Power f n)) = Just(Product der_f) where der_f = get_pow_der var [f]


get_sum_der ::Char -> [ME] -> [ME]
get_sum_der var []     = []
get_sum_der var (x:xs) = [x']++get_sum_der var xs where Just(x') = derivative var (Just x)

get_prod_der :: Char -> [ME] -> [ME]
get_prod_der var []    = []
get_prod_der var (x:xs) = [Product([x', x])]++get_prod_der var xs where Just(x') = derivative var (Just x) 

{-
get_pow_der:: Char -> [ME] -> [ME]
get_pow_der var (Num 1)  = []
get_pow_der var (Power f n)   = [Power f (n-1), (Num n)]++[derivative var f'] where f' = Power f (n-1) 
-}

--Parser 
parseME::[Char] -> Maybe ME
parseME s = 
  case parse_me s of
  Just(e, rest) -> Just(e)
  _             -> Nothing

parse_me::[Char] -> Maybe (ME, [Char])
parse_me s = 
  case parseTerm(s) of
     Just(e, '+':more) -> Just(Sum ([e]++e'), yet_more) where (e', yet_more) = extendME(e, ['+']++more)
     Just(e, '-':more) -> Just(Sum ([e]++[(Neg e')]++e''), even_yet_more) 
       where 
          Just(e', yet_more)=parseTerm(more) 
          (e'', even_yet_more)=extendME(e', yet_more)
     Just(e, [])       -> Just(e, [])
     Just(e, more)     -> Just(e, more)
   

extendME:: (ME, [Char]) -> ([ME], [Char])
extendME (e, []) = ([], [])
extendME (e, c:more_elems) 
    | more_elems /= [] && (c=='+' || c=='-')  = ([e1]++e2, even_yet_more)
    | yet_more == [] && [e1] /= []            = ([e1], yet_more)
    | otherwise           = ([e], more_elems) 
    where 
      Just(e1, yet_more)=parseTerm(more_elems)
      (e2, even_yet_more)=extendME(e1, yet_more)

parseTerm::[Char] -> Maybe (ME, [Char])
parseTerm s =
    case parseFactor(s) of
       Just (e, '*':more_elems) -> Just(Product ([e]++extendTerm(more_elems')), restOfTerm(more_elems')) where more_elems' = '*':more_elems
       Just (e,[])          -> Just(e, [])
       Just (e, more_elems) -> Just(e, more_elems)
       _                    -> Nothing
-- update this to follow extendME     
extendTerm::[Char] -> [ME]
extendTerm [] = []
extendTerm (c:more_elems)
    | more_elems /=[] && c=='*'       = [e]++extendTerm(yet_more)
    | otherwise                       = []
    where Just(e, yet_more) = parseFactor(more_elems)  

restOfTerm:: [Char] -> [Char]
restOfTerm s = drop ((firstIndexNotStar s)) s

firstIndexNotStar::[Char] -> Int
firstIndexNotStar [] = 0
firstIndexNotStar (c1:[]) = 1
firstIndexNotStar (c1:more_elems)
    | c1=='+' || c1=='-'  = 0
    | otherwise            = 1 + firstIndexNotStar(more_elems)


parseFactor:: [Char] -> Maybe (ME, [Char])
parseFactor ('-':more) = 
    case parseElement(more) of
       Just (e, yet_more) -> Just(Neg e', even_yet_more) where Just(e', even_yet_more) =  extendFactor(e, yet_more)
       _                  -> Nothing
parseFactor s =
    case parseElement(s) of
       Just (e, more_elems) -> extendFactor(e, more_elems)
       _                    -> Nothing

extendFactor::( ME, [Char]) -> Maybe (ME, [Char])
extendFactor (e, [])         = Just(e, [])
extendFactor (e1, '*':'*':after_power) =
    case parseNumeral(after_power) of
       Just (e2, more)      -> Just(Power e1 (convertToInt after_power), more)
       _                    -> Nothing
extendFactor (e, more) = Just (e, more)

convertToInt::[Char] -> Int
convertToInt s = read n::Int
    where n = getNumeral s


parseElement:: [Char]-> Maybe (ME, [Char])
parseElement ('(':more) = 
    case parse_me(more) of
       Just (e, ')':yet_more) -> Just(Group e, yet_more)
       _       -> Nothing


parseElement [] = Nothing
parseElement (c:s)
    | c `elem` ['a'..'z']  = parseVariable(s')
    | c `elem` ['0'..'9']  = parseNumeral(s')
    | otherwise            = Nothing
    where s' = c:[] ++ s


parseNumeral:: [Char] -> Maybe (ME, [Char])
parseNumeral s 
    | n /= []   = Just(Num (read n::Int), restOfNum s)
    | otherwise = Nothing 
    where n = getNumeral s

getNumeral:: [Char] -> [Char]
getNumeral (c:s)
    | s == []  && c `elem` ['0'..'9']                = c:[]
    | s == []  && c `elem` ['a'..'z']++['+','-','*', '(', ')'] = []
    | c `elem` ['a'..'z']++['+','-','*', '(', ')'] && s/=[]    = [] 
    | otherwise                                      = c:[] ++ getNumeral(s)

restOfNum:: [Char] -> [Char]
restOfNum s = drop (firstIndexNotNum s) s

firstIndexNotNum:: [Char] -> Int
firstIndexNotNum (c:s) 
    | s == []  && c `elem` ['0'..'9']                = 1
    | s == []  && c `elem` ['a'..'z']++['+','-','*','(',')'] = 0
    | c `elem` ['a'..'z']++['+','-','*','(',')'] && s/=[]    = 0
    | otherwise                                      = 1 + firstIndexNotNum s

parseVariable:: [Char] -> Maybe (ME, [Char])
parseVariable (c:s)
    | c `elem` ['+','-','*'] = Nothing
    | otherwise = Just (Var c, s)






