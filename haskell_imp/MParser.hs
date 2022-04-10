module MParser where

import System.IO 
import System.IO( isEOF )
import Data.List

-- STEPS FOR MAIN
-- check if everything is in the correct order
-- record all data regardless if it is correct or not
-- check if data recorded is correct
    -- if its correct -> move on
    -- else fail

nameLabel   = "Name:"
fpaLabel    = "forced partial assignment:"
fmLabel     = "forbidden machine:"
tntLabel    = "too-near tasks:"
mpLabel     = "machine penalties:"
tnpLabel    = "too-near penalities"




remove :: Char -> [Char] -> [Char]
remove element list = filter (\e -> e/=element) list

hasDupe :: [(Int, Char)] -> Bool
hasDupe [] = False
hasDupe (x:xs)
    |hasDupe2 x xs = True
    |otherwise = hasDupe xs
    where   hasDupe2 :: (Int, Char) -> [(Int, Char)] -> Bool
            hasDupe2 key [] = False
            hasDupe2 key (y:ys)
                |fst key == fst y || snd key == snd y = True
                |otherwise = hasDupe2 key ys





{-
     reads input file and places every line into its own index in a list
     line 1 = index 0, line 2 = index 1, etc..
-} 
myReadFile :: String -> IO [String]
myReadFile fName = do
    file <- openFile fName ReadMode
    -- create list containing lines of input file
    makeList file 
    where   makeList :: Handle -> IO [String]  
            makeList f = do
                eof <- hIsEOF f
                -- loops through file lines until we reach end of the file
                loopFile eof
                where   loopFile :: Bool -> IO [String]
                        -- if we are not at EOF
                        loopFile False = do
                            x <- hGetLine f
                            xs <- makeList f
                            -- if current line is empty, do not record it
                            let temp = filter (/= '\r') (filter (/= ' ') x)
                            if length temp == 0 then return xs
                            -- append x to new list
                            else return ((filter (/= '\r') x):xs)
                        -- if we are at the EOF
                        loopFile True = return []


noTrailSpace :: String -> String
noTrailSpace str
    |last str == ' ' = noTrailSpace (init str)
    |otherwise = str
    
{-
    function to convert character to integer
-}  
myCharToInt :: Char -> Int
myCharToInt x 
    | x == '1' = 1
    | x == '2' = 2
    | x == '3' = 3
    | x == '4' = 4
    | x == '5' = 5
    | x == '6' = 6
    | x == '7' = 7
    | x == '8' = 8
    | otherwise = -1

myTaskToInt :: Char -> Int
myTaskToInt x 
    | x == 'A' = 1
    | x == 'B' = 2
    | x == 'C' = 3
    | x == 'D' = 4
    | x == 'E' = 5
    | x == 'F' = 6
    | x == 'G' = 7
    | x == 'H' = 8
    | otherwise = -1

myCorrectTask :: Char -> Bool
myCorrectTask x
    | x == 'A' = True
    | x == 'B' = True
    | x == 'C' = True
    | x == 'D' = True
    | x == 'E' = True
    | x == 'F' = True
    | x == 'G' = True
    | x == 'H' = True
    | otherwise = False

correctOrder :: IO [String] -> IO Bool
correctOrder    file = do
    list <- file
    if correctOrder2 list 0 then return True
    else return False
    where   correctOrder2 :: [String] -> Int -> Bool
            correctOrder2   [] state
                |state == 6 = True
                |otherwise = False
            correctOrder2   (x:xs) state
                |noTrailSpace x == nameLabel && state == 0 = correctOrder2 xs 1 
                |noTrailSpace x == fpaLabel && state == 1 = correctOrder2 xs 2
                |noTrailSpace x == fmLabel && state == 2 = correctOrder2 xs 3
                |noTrailSpace x == tntLabel && state == 3 = correctOrder2 xs 4
                |noTrailSpace x == mpLabel && state == 4 = correctOrder2 xs 5
                |noTrailSpace x == tnpLabel && state == 5 = correctOrder2 xs 6
                |otherwise = correctOrder2 xs state
 
{-
    function to check if a character is an element of a string
-}                
myElem :: Char -> String -> Bool
myElem e [] = False
myElem e (first:rest)
    | first == e = True
    | otherwise = myElem e rest

mySubstring :: String -> String -> Int -> Int -> [Char]
mySubstring x res iStrt iEnd
    |iStrt == iEnd = res ++ []
    |iStrt < iEnd = mySubstring x (res ++ [(x!!iStrt)]) (iStrt + 1) iEnd
    
myStringToInt :: String -> Int -> Int
myStringToInt [] ans = ans
myStringToInt (x:xs) ans = myStringToInt xs ((ans * 10) + digitToInt x)

digitToInt :: Char -> Int
digitToInt x 
    | x == '0' = 0
    | x == '1' = 1
    | x == '2' = 2
    | x == '3' = 3
    | x == '4' = 4
    | x == '5' = 5
    | x == '6' = 6
    | x == '7' = 7
    | x == '8' = 8
    | x == '9' = 9
    | otherwise = -1

onlyNumbers :: String -> Bool
onlyNumbers [] = True
onlyNumbers (x:xs) 
    | x == '0' = onlyNumbers xs
    | x == '1' = onlyNumbers xs
    | x == '2' = onlyNumbers xs
    | x == '3' = onlyNumbers xs
    | x == '4' = onlyNumbers xs
    | x == '5' = onlyNumbers xs
    | x == '6' = onlyNumbers xs
    | x == '7' = onlyNumbers xs
    | x == '8' = onlyNumbers xs
    | x == '9' = onlyNumbers xs
    | otherwise = False

arrayValid :: IO [(Int, Char)] -> IO Bool
arrayValid x = do
    list <- x
    if fst (last list) == -1 then return False
    else return True

tntArrayValid :: IO [(Char, Char)] -> IO Bool
tntArrayValid  x = do
    list <- x
    if fst (last list) == 'z' then return False
    else return True

tnpArrayValid :: IO [(Char, Char, Int)] -> IO Bool
tnpArrayValid x = do
    list <- x
    if first (last list) == 'z' then return False
    else return True 
    where   first :: (Char, Char, Int) -> Char
            first (a,_,_) = a

parseName   :: IO [String] -> IO Bool
parseName   file = do
    list <- file
    if parseName2 list then return True
    else return False
    where   parseName2 :: [String] -> Bool
            parseName2 file
                |file!!0 == nameLabel && not (myElem ' ' (file!!1)) && file!!2 == fpaLabel = True
                |otherwise = False
        
parseFPA    :: IO [String] -> IO [(Int, Char)]
parseFPA    file = do
    list <- file
    let out = parseFPA2 list 0
    return out
    where   parseFPA2 :: [String] -> Int -> [(Int, Char)]
            parseFPA2 [] state = [(-1, 'x')]
            parseFPA2 (x:xs) state
                |x == fpaLabel = parseFPA2 xs 1
                |state == 1 && x == fmLabel = []
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && (filter (/= ' ') x)!!4 == ')' && (filter (/= ' ') x)!!2 == ',' && (myCharToInt ((filter (/= ' ') x)!!1) == -1 || not (myCorrectTask ((filter (/= ' ') x)!!3))) = [(-1, 'z')] 
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && (filter (/= ' ') x)!!4 == ')' && (filter (/= ' ') x)!!2 == ',' && myCharToInt ((filter (/= ' ') x)!!1) /= -1 && myCorrectTask ((filter (/= ' ') x)!!3) = (myCharToInt ((filter (/= ' ') x)!!1), (filter (/= ' ') x)!!3) : parseFPA2 xs 1 
                |otherwise = parseFPA2 xs 0

parseFM    :: IO [String] -> IO [(Int, Char)]
parseFM    file = do
    list <- file
    let out = parseFM2 list 0
    return out
    where   parseFM2 :: [String] -> Int -> [(Int, Char)]
            parseFM2 [] state = [(-1, 'x')]
            parseFM2 (x:xs) state
                |x == fmLabel = parseFM2 xs 1
                |state == 1 && x == tntLabel = []
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && (filter (/= ' ') x)!!4 == ')' && (filter (/= ' ') x)!!2 == ',' && (myCharToInt ((filter (/= ' ') x)!!1) == -1 || not (myCorrectTask ((filter (/= ' ') x)!!3))) = [(-1, 'z')] 
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && (filter (/= ' ') x)!!4 == ')' && (filter (/= ' ') x)!!2 == ',' && myCharToInt ((filter (/= ' ') x)!!1) /= -1 && myCorrectTask ((filter (/= ' ') x)!!3) = (myCharToInt ((filter (/= ' ') x)!!1), (filter (/= ' ') x)!!3) : parseFM2 xs 1 
                |otherwise = parseFM2 xs 0

parseTNT    :: IO [String] -> IO [(Char, Char)]
parseTNT    file = do
    list <- file
    let out = parseTNT2 list 0
    return out
    where   parseTNT2 :: [String] -> Int -> [(Char, Char)]
            parseTNT2 [] state = [('x', 'x')] 
            parseTNT2 (x:xs) state
                |x == tntLabel = parseTNT2 xs 1
                |state == 1 && x == mpLabel = []
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && (filter (/= ' ') x)!!4 == ')' && (filter (/= ' ') x)!!2 == ',' && (not (myCorrectTask ((filter (/= ' ') x)!!1)) || not (myCorrectTask ((filter (/= ' ') x)!!3))) = [('z', 'z')] 
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && (filter (/= ' ') x)!!4 == ')' && (filter (/= ' ') x)!!2 == ',' && myCorrectTask ((filter (/= ' ') x)!!1) && myCorrectTask ((filter (/= ' ') x)!!3) = ((filter (/= ' ') x)!!1, (filter (/= ' ') x)!!3) : parseTNT2 xs 1 
                |otherwise = parseTNT2 xs 0

parseMP     :: IO [String] -> IO [[Int]]
parseMP     file = do
    list <- file
    let out = parseMP2 list 0
    return out
    where   parseMP2 :: [String] -> Int -> [[Int]]
            parseMP2 (x:xs) state
                |x == mpLabel = parseMP2 xs 1 
                |state == 9 && x == tnpLabel = []
                |state /= 9 && x == tnpLabel = [[-1]]
                |state >= 1 && not (length(words x) == 8) = [[-1]]
                |state >= 1 && not (allInt (words x)) = [[-2]]
                |state >= 1 && length(words x) == 8 && allInt (words x) = convertRow (words x) : parseMP2 xs (state + 1) 
                |otherwise = parseMP2 xs 0 
                where   allInt :: [String] -> Bool
                        allInt [] = True 
                        allInt (x:xs) 
                            | onlyNumbers x = allInt xs
                            | otherwise = False
                        convertRow :: [String] -> [Int]
                        convertRow [] = []
                        convertRow (x:xs) = (myStringToInt x 0) : convertRow xs 

parseTNP    :: IO [String] -> IO [(Char, Char, Int)]
parseTNP    file = do
    list <- file
    let out = parseTNP2 list 0
    return out
    where   parseTNP2 :: [String] -> Int -> [(Char, Char, Int)]
            parseTNP2 [] state
                |state == 1 = []
                |otherwise = [('z', 'z', -3)]
            parseTNP2 (x:xs) state
                |x == tnpLabel = parseTNP2 xs 1
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && last (filter (/= ' ') x) == ')' && (filter (/= ' ') x)!!2 == ',' && (filter (/= ' ') x)!!4 == ',' &&  ( not (myCorrectTask ((filter (/= ' ') x)!!1)) || not (myCorrectTask ((filter (/= ' ') x)!!3)) ) = [('z', 'z', -1)]
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && last (filter (/= ' ') x) == ')' && (filter (/= ' ') x)!!2 == ',' && (filter (/= ' ') x)!!4 == ',' && not (onlyNumbers (mySubstring x "" 5 ((length (filter (/= ' ') x))-1))) = [('z', 'z', -2)]
                |state == 1 && (filter (/= ' ') x)!!0 == '(' && last (filter (/= ' ') x) == ')' && (filter (/= ' ') x)!!2 == ',' && (filter (/= ' ') x)!!4 == ',' && myCorrectTask ((filter (/= ' ') x)!!1) && myCorrectTask ((filter (/= ' ') x)!!3) && onlyNumbers (mySubstring x "" 5 ((length (filter (/= ' ') x))-1)) = ((filter (/= ' ') x)!!1     , (filter (/= ' ') x)!!3     , myStringToInt (mySubstring x "" 5 ((length (filter (/= ' ') x)) - 1)) 0) : parseTNP2 xs 1
                |otherwise = parseTNP2 xs 0

mainParse   :: String -> IO Int
mainParse   fName = do
    goodOrder <- correctOrder (myReadFile fName)
    name <- parseName (myReadFile fName)
    fpa <- parseFPA (myReadFile fName)
    fm <- parseFM (myReadFile fName)
    tnt <- parseTNT (myReadFile fName)
    mp <- parseMP (myReadFile fName)
    tnp <- parseTNP (myReadFile fName)
    
    {-
    0 = Error while parsing input file
    -1 = invalid machine/task
    -2 = partial assignment error
    -3 = machine penalty error
    -4 = invalid penalty
    -5 = invalid task
    -}

    if not goodOrder then return (0) 
    else if not name then return (0) 
    else if hasDupe fpa then return (-2) 
    else if length fpa /= 0 && last fpa == (-1, 'z') then return (-1) 
    else if length fpa /= 0 && last fpa == (-1, 'x') then return (0)
    else if length fm /= 0 && last fm == (-1, 'z') then return (-1)
    else if length fm /= 0 && last fm == (-1, 'x') then return (0)
    else if length tnt /= 0 && last tnt == ('z', 'z') then return (-1)
    else if length tnt /= 0 && last tnt == ('x', 'x') then return (0)
    else if last mp == [-1] then return (-3) 
    else if last mp == [-2] then return (-4) 
    else if length tnp /= 0 && last tnp == ('z', 'z', -1) then return (-5) 
    else if length tnp /= 0 && last tnp == ('z', 'z', -2) then return (-4) 
    else if length tnp /= 0 && last tnp == ('z', 'z', -3) then return (0) 
    else return 1


