import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)
data   ME = Num Int 
          | Var Char
          | Group ME
          | Sum [ME]
          | Product [ME] 
          | Power ME Int
          | Neg ME
          deriving (Show, Ord, Eq)


-- Main Program
parse_deriv_simp_unparse::Char-> [Char] -> Maybe [Char]
parse_deriv_simp_unparse v s =
  case parseME s of
    Just e -> Just (unparseME (simplifyME (derivative v e)))
    _ -> Nothing

main = do
  [[v], expr] <- getArgs
  case parse_deriv_simp_unparse v expr of
    Just s -> hPutStrLn stdout s
    _ -> hPutStrLn stdout "Parse failure"

-- Simplifier
simplifyME :: ME -> ME
simplifyME e = e

-- Unparser
unparseME :: ME -> [Char]
unparseME (Num n)= show n
unparseME (Var v) = [v]
unparseME (Neg n) = "-("++(unparseME n)++")"
unparseME (Power f e) = unparseME(f)++"**"++(show e)
unparseME (Sum s)
   |   tail s /= []  =  unparseME(head s)++"+"++unparseME(Sum (tail s))
   |   otherwise     =  unparseME(head s)
unparseME (Product s) 
   |   tail s /= []  =  unparseME(head s)++"*"++unparseME(Product (tail s))
   |   otherwise     =  unparseME(head s)


-- Derivative
derivative:: Char -> ME -> ME
derivative var (Num n) = (Num 0)
derivative var (Var v)
    | v == var   = (Num 1)
    | otherwise  = (Num 0)
derivative var (Neg n) = (Neg neg) where neg = derivative var n 
derivative var (Sum s) = (Sum der_s) where der_s  = get_sum_der var s  
derivative var (Product p) = (Sum der_p) where der_p = get_prod_der var p
derivative var (Power (Var x) n)
     | x == var && ((n-1)>1 || (n-1) < 1)  = (Product [(Power (Var x) (n-1)), (Num n)])
     | x == var && ((n-1) == 1)        = (Product[(Num 2), (Var x)]) 
     | otherwise = (Num 0) 

get_sum_der ::Char -> [ME] -> [ME]
get_sum_der var []     = []
get_sum_der var (x:xs) = [x']++get_sum_der var xs where (x') = derivative var x

get_prod_der :: Char -> [ME] -> [ME]
get_prod_der var []    = []
get_prod_der var (x:xs) 
     |  xs /= []        = [Product([x']++xs)]++[Product(xs'++[x])] 
     |   otherwise       = [Product([x'])]
      where 
        x' = derivative var x
        xs' = get_prod_der var xs


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
       Just (e, '*':more_elems) -> Just(Product ([e]++e'), yet_more) where (e', yet_more) = extendTerm(e, '*':more_elems)
       Just (e,[])          -> Just(e, [])
       Just (e, more_elems) -> Just(e, more_elems)
       _                    -> Nothing

-- update this to follow extendME     
extendTerm::(ME, [Char]) -> ([ME], [Char])
extendTerm (e,[]) = ([], [])
extendTerm (e, c:more_elems)
    | more_elems /=[] && c=='*'                = ([e1]++e2, even_yet_more)
    | yet_more == [] && [e1] /= [] && c=='*'   = ([e1], [])
    | otherwise                                = ([], c:more_elems)
    where 
        Just(e1, yet_more) = parseFactor(more_elems)
        (e2, even_yet_more)=extendTerm(e1, yet_more)        


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

