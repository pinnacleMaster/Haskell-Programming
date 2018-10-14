data RE = Epsilon | Ch Char | Seq RE RE | Alt RE RE | Star RE | Group RE deriving Show

match :: RE -> [Char] -> Bool
splits :: [Char] -> [([Char], [Char])]
combine_all :: Char -> [([Char], [Char])] -> [([Char], [Char])]
match_any_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_nonempty_split :: RE -> RE -> [([Char], [Char])] -> Bool

match Epsilon s = s == ""
match (Ch a) "" = False
match (Ch a) (c : more_chars) = a == c && more_chars == []
match (Alt r1 r2) string = match r1 string || match r2 string
match (Seq r1 r2) string = match_any_split r1 r2 (splits string)
match (Star r1) "" = True
match (Star r1) s = match_any_nonempty_split r1 (Star r1) (splits s)

splits "" = [("", "")]
splits (c : chars) = combine_all c (splits chars)

combine_all c [] = []
combine_all c (("", s): more) = ([c], s) : ("", c:s) : (combine_all c more)
combine_all c ((a:as, s): more) = (c:a:as, s) : (combine_all c more)

match_any_split r1 r2 [] = False
match_any_split r1 r2 ((s1, s2) : more_splits) 
   | match r1 s1 && match r2 s2 = True
   | otherwise = match_any_split r1 r2 more_splits 

match_any_nonempty_split r1 r2 [] = False
match_any_nonempty_split r1 r2 ((s1, s2) : more_splits) 
   | s1 /= "" && match r1 s1 && match r2 s2 = True
   | otherwise = match_any_nonempty_split r1 r2 more_splits 
