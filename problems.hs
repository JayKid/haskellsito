repli :: Int -> String -> String

repli times [] = []
repli times (x:xs) = (take times (repeat x)) ++ (repli times xs)

repli2 xs times = concatMap (take times . repeat) xs