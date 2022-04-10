module MTree where

import MParser
import MConstraints

tasks = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']

        -- filename -> returns best path
testTest    :: String -> [Char] -> IO Int
testTest    fName tree = do
    cFPA <- parseFPA (myReadFile fName)
    cFM <- parseFM (myReadFile fName)
    cTNT <- parseTNT (myReadFile fName)
    cMP <- parseMP (myReadFile fName)
    cTNP <- parseTNP (myReadFile fName)
    let cnstrnt = (cFPA, cFM, cTNT, cMP, cTNP)
    let testResults = doTests tree cnstrnt
    return testResults

makeTree    :: String -> IO ([Char], Int)
makeTree    fName  = do
    cFPA <- parseFPA (myReadFile fName)
    cFM <- parseFM (myReadFile fName)
    cTNT <- parseTNT (myReadFile fName)
    cMP <- parseMP (myReadFile fName)
    cTNP <- parseTNP (myReadFile fName)
    let cnstrnt = (cFPA, cFM, cTNT, cMP, cTNP)
    let result = makeTree2 cnstrnt ([], (maxBound :: Int)) ([], 0) tasks
    return result
    where   makeTree2 :: ([(Int, Char)], [(Int, Char)], [(Char, Char)], [[Int]], [(Char, Char, Int)]) -> ([Char], Int) -> ([Char], Int) -> [Char] -> ([Char], Int)
            makeTree2 aids prev curr [] = prev
            makeTree2 aids prev curr (x:xs)
                |elem x (fst curr) = makeTree2 aids prev curr xs
                |(doTests ((fst curr)++[x]) aids) < 0 = makeTree2 aids prev curr xs
                |(doTests ((fst curr)++[x]) aids) + (snd curr) > (snd prev) = prev
                |length ((fst curr)++[x]) == 8 && (snd curr + (doTests ((fst curr)++[x]) aids)) < (snd prev) = (((fst curr)++[x]), (snd curr + (doTests ((fst curr)++[x]) aids)))
                |length ((fst curr)++[x]) == 8 && (snd curr + (doTests ((fst curr)++[x]) aids)) >= (snd prev) = prev
                |snd (makeTree2 aids prev (((fst curr)++[x]), ((snd curr) + doTests ((fst curr)++[x]) aids)) tasks) < (snd prev) = makeTree2 aids (makeTree2 aids prev (((fst curr)++[x]), ((snd curr) + doTests ((fst curr)++[x]) aids)) tasks) curr xs
                |snd (makeTree2 aids prev (((fst curr)++[x]), ((snd curr) + doTests ((fst curr)++[x]) aids)) tasks) >= (snd prev) = makeTree2 aids prev curr xs

doTests :: [Char] -> ([(Int, Char)], [(Int, Char)], [(Char, Char)], [[Int]], [(Char, Char, Int)]) -> Int
doTests path cnst
    |not (forcedPartial (length path, last path) (first cnst)) = (minBound :: Int)
    |forbidden (length path, last path) (second cnst) = (minBound :: Int)
    |length path < 2 = ((macPen (length path) (myTaskToInt (last path)) (fourth cnst)) + (tooNearPenalties (init path) (last path) (fifth cnst) (fifth cnst)))
    |length path == 8 && too_near ((head path), (last path)) (third cnst) = (minBound :: Int)
    |too_near ((last (init path)), (last path)) (third cnst) = (minBound :: Int)
    |otherwise = ((macPen (length path) (myTaskToInt (last path)) (fourth cnst)) + (tooNearPenalties (init path) (last path) (fifth cnst) (fifth cnst)))
    where   first :: (a,b,c,d,e) -> a
            first (fst, _ , _ , _ , _) = fst
            second :: (a,b,c,d,e) -> b
            second (_, scnd , _ , _ , _) = scnd
            third :: (a,b,c,d,e) -> c
            third (_, _ , thrd , _ , _) = thrd
            fourth :: (a,b,c,d,e) -> d
            fourth (_ , _ , _ , frth , _) = frth
            fifth :: (a,b,c,d,e) -> e
            fifth (_, _ , _ , _ , ffth) = ffth

treeToString :: ([Char], Int) -> String -> String
treeToString tree []
    |snd tree == (maxBound :: Int) && length (fst tree) == 0 = "No valid solution possible!"
    |otherwise = treeToString tree "Solution "
treeToString tree sln
    |length (fst tree) == 1 = treeToString (tail (fst tree), snd tree) (sln ++ [head (fst tree)] ++ [';'])
    |length (fst tree) /= 0 = treeToString (tail (fst tree), snd tree) (sln ++ [head (fst tree)] ++ [' '])
    |otherwise = (sln ++ " Quality: ") ++ (show (snd tree))
        





        






