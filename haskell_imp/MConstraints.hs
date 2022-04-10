module MConstraints where

-- FORCED PARTIAL ASSIGNMENT
forcedPartial :: (Int, Char) -> [(Int, Char)] -> Bool
forcedPartial n [] = True
forcedPartial n (x:xs)
    | (fst n == fst x) && (snd n /= snd x) = False
    | (fst n /= fst x) && (snd n == snd x) = False
    | (fst n == fst x) && (snd n == snd x) = True
    | otherwise = forcedPartial n xs



-- FORBIDDEN MACHINE
forbidden :: (Int, Char) -> [(Int, Char)] -> Bool
forbidden n [] = False
forbidden n (first:rest)
    | (fst n == fst first) && (snd n == snd first) = True 
    | otherwise = forbidden n rest

-- TOO NEAR TASKS
too_near :: (Char, Char) -> [(Char, Char)] -> Bool
too_near n [] = False 
too_near n (x:xs)
    |(fst n == fst x) && (snd n == snd x) = True 
    | otherwise = too_near n xs



-- MACHINE PENALTIES
macPen :: Int -> Int -> [[Int]] -> Int
macPen _ _ [] = -1
macPen a b inp
    | length inp == 8 = macPen2 a b inp
    | otherwise = -1
    where   macPen2 :: Int -> Int -> [[Int]] -> Int
            macPen2 _ _ [] = -1
            macPen2 m t (x:xs)
                | length x /= 8 = -1
                | m == 8 - length xs = macPen3 t x
                | otherwise = macPen2 m t xs
                where   macPen3 :: Int -> [Int] -> Int
                        macPen3 _ [] = -1
                        macPen3 task (y:ys)
                            | task == 8 - length ys = y
                            | otherwise = macPen3 task ys

-- A B C D E H F G (F,G,8) (G, A, 8)

-- TOO NEAR PENALTIES
tooNearPenalties :: [Char] -> Char -> [(Char, Char, Int)] -> [(Char, Char, Int)] -> Int
tooNearPenalties tree child [] tnp = 0
tooNearPenalties tree child (x:xs) tnp
    |length tree == 7 && child == first x && head tree == second x = (third x) + (tooNearPenalties [last tree] child tnp tnp)
    |length tree == 7 && last tree == first x && child == second x = (third x) + (tooNearPenalties tree child xs tnp)
    |length tree == 7 = tooNearPenalties tree child xs tnp
    |length tree >= 1 && last tree == first x && child == second x = third x
    |otherwise = tooNearPenalties tree child xs tnp
    where   third :: (Char, Char, Int) -> Int
            third (_,_,z) = z
            first :: (Char, Char, Int) -> Char
            first (a,_,_) = a
            second :: (Char, Char, Int) -> Char
            second (_,b,_) = b