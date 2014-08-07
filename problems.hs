-- http://www.haskell.org/haskellwiki/99_questions

repli :: Int -> String -> String

repli times [] = []
repli times (x:xs) = (take times (repeat x)) ++ (repli times xs)

repli2 xs times = concatMap (take times . repeat) xs

dropnth :: [a] -> Int -> [a]

dropnth [] _ = []
dropnth l n = (take (n-1) l) ++ dropnth (drop n l) n