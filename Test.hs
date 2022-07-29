getPlayer :: String -> Char
getPlayer (xs : []) = xs
getPlayer (x : xs) = getPlayer xs

allP :: ((Int, Int), (Int, Int)) -> Int
allP x =
  let a1 = fst (fst x)
   in let a2 = fst (snd x)
       in let a3 = snd (fst x)
           in a1 + a2 + a3
