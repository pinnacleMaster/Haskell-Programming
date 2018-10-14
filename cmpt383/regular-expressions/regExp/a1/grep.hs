import Data.Bits
import Data.Array
import Data.Char
--
-- Glushkov Automaton Regular Expressions in Haskell (Fast)
--

-- 1. An algebraic data type for regular expressions

data RE = Epsilon | Ch Char | Seq RE RE | Alt RE RE | Star RE | Group RE deriving Show

-- 2. Parser to convert string into regular expression

parseRE :: [Char] -> Maybe (RE, [Char])
parseSeq :: [Char] -> Maybe (RE, [Char])
parseItem :: [Char] -> Maybe (RE, [Char])
parseElement :: [Char] -> Maybe (RE, [Char])
parseChar :: [Char] -> Maybe (RE, [Char])

parseChar [] = Nothing
parseChar (b:c:s)
  | (c == '|' || c == '*' || c == '(' || c == ')') && b /= '\\'  = Nothing
  | (c == '|' || c == '*' || c == '(' || c == ')') && b == '\\'  = Just ((Ch c), s)
  | otherwise                                      = Just ((Ch c), s)

parseElement ('(':more) =
    case parseRE(more) of
        Just (re, ')':yet_more) -> Just(Group re, yet_more)
        _ -> Nothing
parseElement s = parseChar s

parseItem s =
   case parseElement(s) of
        Just (re, '*':more) -> Just (Star re, more)
        Just (re, more) -> Just (re, more)
        _ -> Nothing

extendSeq :: (RE, [Char]) -> Maybe (RE, [Char])

parseSeq s =
    case parseItem(s) of
        Just (r, more_chars) -> extendSeq(r, more_chars)
        _ -> Nothing

extendSeq (e1, after1) =
    case parseItem(after1) of
        Just(e2, more) -> extendSeq(Seq e1 e2, more)
        _ -> Just(e1, after1)

extendRE :: (RE, [Char]) -> Maybe (RE, [Char])
parseRE s =
    case parseSeq(s) of
        Just (r, more_chars) -> extendRE(r, more_chars)
        _ -> Nothing

extendRE (e1, []) = Just (e1, [])
extendRE (e1, '|' : after_bar) =
    case parseSeq(after_bar) of
        Just(e2, more) -> extendRE(Alt e1 e2, more)
        _ -> Nothing
extendRE(e1, c:more) = Just (e1, c:more)

parseMain :: [Char] -> Maybe RE

parseMain s = case parseRE s of
    Just (e, []) -> Just e
    _ -> Nothing




-- 3. Glushkov Automaton

countCh :: RE -> Int
countCh Epsilon = 0
countCh (Ch c) = 1
countCh (Seq r1 r2) = (countCh r1) + (countCh r2)
countCh (Alt r1 r2) = (countCh r1) + (countCh r2)
countCh (Star r) = countCh r
countCh (Group r) = countCh r

matchesEmpty :: RE -> Bool
matchesEmpty Epsilon = True
matchesEmpty (Ch c) = False
matchesEmpty (Seq r1 r2) = (matchesEmpty r1) && (matchesEmpty r2)
matchesEmpty (Alt r1 r2) = (matchesEmpty r1) || (matchesEmpty r2)
matchesEmpty (Star r) = True
matchesEmpty (Group r) = matchesEmpty r

firstWithIndex :: Int -> RE -> [Int]
firstWithIndex i Epsilon = []
firstWithIndex i (Ch c) = [i]
firstWithIndex i (Seq r1 r2)
   | matchesEmpty r1 = (firstWithIndex i r1) ++ (firstWithIndex (i + countCh r1) r2)
   | otherwise       = (firstWithIndex i r1)
firstWithIndex i (Alt r1 r2) = (firstWithIndex i r1) ++ (firstWithIndex (i + countCh r1) r2)
firstWithIndex i (Star r) = firstWithIndex i r
firstWithIndex i (Group r) = firstWithIndex i r

firstSym :: Maybe RE -> [Int]
firstSym r = case r of
    Just r ->firstWithIndex 1 r
    Nothing -> []

finals_ix :: Int -> RE -> [Int]
finals_ix i Epsilon = []
finals_ix i (Ch c) = [i]
finals_ix i (Seq r1 r2)
   | matchesEmpty r2 = (finals_ix i r1) ++ (finals_ix (i + countCh r1) r2)
   | otherwise       = finals_ix (i + countCh r1) r2
finals_ix i (Alt r1 r2) = (finals_ix i r1) ++ (finals_ix (i + countCh r1) r2)
finals_ix i (Star r) = finals_ix i r
finals_ix i (Group r) = finals_ix i r

finalSyms :: Maybe RE -> [Int]
finalSyms r = case r of
    Just r -> finals_ix 1 r
    Nothing -> []

symPairs :: Maybe RE -> [(Int,Int)]
symPairs r = case r of 
    Just r -> pairs_ix 1 r
    Nothing -> []

pairs_ix :: Int -> RE -> [(Int,Int)]
pairs_ix i Epsilon = []
pairs_ix i (Ch c) = []
pairs_ix i (Seq r1 r2) =
    let i2 = i + countCh r1 in
    (pairs_ix i r1) ++ (pairs_ix i2 r2) ++ allPairs (finals_ix i r1) (firstWithIndex i2 r2)
pairs_ix i (Alt r1 r2) = (pairs_ix i r1) ++ (pairs_ix (i + countCh r1) r2)
pairs_ix i (Star r) = (pairs_ix i r) ++ allPairs (finals_ix i r) (finals_ix i r)
pairs_ix i (Group r) = pairs_ix i r

allPairs :: [Int] -> [Int] -> [(Int,Int)]
allPairs list1 list2 = [(e1, e2) | e1 <- list1, e2 <- list2]

positions_vec :: [Int] -> Int
positions_vec posns = foldr (.|.) 0 (map (2^) posns)

{-|
ch_positions :: Char -> Int -> RE -> [Int]
ch_positions c i Epsilon = []
ch_positions c i (Ch c0)
    | c == c0   = [i]
    | otherwise = []
ch_positions c i (Seq r1 r2) = (ch_positions c i r1) ++ (ch_positions c (i + countCh r1) r2)
ch_positions c i (Alt r1 r2) = (ch_positions c i r1) ++ (ch_positions c (i + countCh r1) r2)
ch_positions c i (Star r) = (ch_positions c i r)
ch_positions c i (Group r) = (ch_positions c i r)

tableB r = listArray (0, 127) [positions_vec(ch_positions (Ch c) 1 r) | c <- [0..127]]

transition_vec :: Int -> Int -> [(Int, Int)] -> Int
transition_vec initial_vec state_vec []
    | (state_vec .&. 1) > 0 = initial_vec
    | otherwise           = 0
transition_vec initial_vec state_vec ((s, t) : more) 
    | 2^s .&. state_vec > 0  = 2^t .|. (transition_vec initial_vec state_vec more)
    | otherwise     = transition_vec initial_vec state_vec more

tableD :: Maybe RE -> Array Int Int
tableD r = 
    let initialVec = positions_vec (firstSym r)
        pairs = symPairs r
        stateMax = 2^(1 + countCh r) - 1 in
    listArray (0, stateMax) [transition_vec initialVec s pairs | s <- [0..stateMax]]

vectorF :: RE -> Int
vectorF r 
    | matchesEmpty r  = positions_vec (0: (finalSyms r))
    | otherwise       = positions_vec (finalSyms r)

-- matching 

match_NFA :: Int -> (Array Int Int, Array Int Int, Int) -> [Char] -> Bool
match_NFA s (b, d, f) [] = (s .&. f) > 0
match_NFA s (b, d, f) (ch:more) = match_NFA ((b!(ord ch)) .&. (d!s)) (b, d, f) more

match r string = match_NFA 1 (tableB r, tableD r, vectorF r) string

-- searching

tableB_search r = listArray (0, 127) [positions_vec (ch_positions (Ch c) 1 r) .|. 1| c <- [0..127]]

tableD_search r = 
    let initialVec = positions_vec (firstSym r)
        pairs = symPairs r
        stateMax = 2^(1 + countCh r) - 1 in
    listArray (0, stateMax) [(transition_vec initialVec s pairs .|. 1) | s <- [0..stateMax]]

search_NFA :: Int -> (Array Int Int, Array Int Int, Int) -> [Char] -> Bool
search_NFA s (b, d, f) [] = (s .&. f) > 0
search_NFA s (b, d, f) (c:more) 
    | (s .&. f) > 0    = True 
    | otherwise        = search_NFA ((b!(ord c)) .&. (d!s) .|. 1) (b, d, f) more

search1 r string = search_NFA 1 (tableB_search r, tableD_search r, vectorF r) string
-}








-- foldr -- fold applied with right associativity

