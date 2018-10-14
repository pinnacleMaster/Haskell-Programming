
data   ME = Num Int 
          | Var Char
          | Group ME
          | Sum [ME]
          | Product [ME] 
          | Power ME Int
          | Neg ME
          deriving (Show, Ord, Eq)


-- Parser 

parseME::[Char] -> Maybe (ME, [Char])
parseME s = 
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
    | more_elems /= [] && (c=='+' || c=='-')  = ([e1], even_yet_more)
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
    case parseME(more) of
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

