module Main where

main :: IO ()
main = do
    let id' = [Binder, Variable 1]
    let false = [Binder, Binder, Variable 1]
    let pair = [Binder, Variable 1, Variable 2, Variable 3]
    let debrsub = [Binder, Variable 5, Variable 1]
    let debrtest = [Binder, Binder,Variable 4, Variable 2, Inner [Binder, Variable 1,Variable 3]]
    print pair
    print (findOccurrence pair)
    print false
    print (findOccurrence false)
    print id'
    print (findOccurrence id')
    print debrtest
    putTerm (substitute (findOccurrence debrtest) debrsub )

data DeBruijn = Binder | Variable Int | Substitute |Inner DeBrTerm deriving (Read)
type DeBrTerm = [DeBruijn]
type DeBrTransform = DeBrTerm -> DeBrTerm -> DeBrTerm

instance Show DeBruijn where
    show Binder     = "\x03BB"
    show (Variable i) = show i
    show Substitute = "\x2588"
    show (Inner t) = " (" ++ showTerm t ++ ")"

showTerm :: DeBrTerm -> String
showTerm = foldr ((++) . show) ""

putTerm :: DeBrTerm -> IO ()
putTerm ts = putStrLn $ showTerm ts


-- Terms without binder in the beginning are not processed correctly
findOccurrence :: DeBrTerm -> DeBrTerm
findOccurrence [] = []
findOccurrence (_:xs) = findOccurrence' xs 1

findOccurrence' :: DeBrTerm -> Int -> DeBrTerm
findOccurrence' [] _ = []
findOccurrence' (x:xs) c
    | Binder <- x = x : findOccurrence' xs (c+1)
    | Inner t <- x = Inner (findOccurrence' t c) : findOccurrence' xs c
    | Variable i <- x
    , i == c
    = Substitute : findOccurrence' xs c
    | Variable i <- x
    , i > c
    = Variable (i-1) : findOccurrence' xs c
    | otherwise = x : findOccurrence' xs c

getMaxVariable :: DeBrTerm -> Int
getMaxVariable = getMaxVariable' 0

getMaxVariable' :: Int -> DeBrTerm -> Int
getMaxVariable' b [] = b
getMaxVariable' b (l:ls)
    | Variable i <- l = getMaxVariable' (max b i) ls
    | Inner t <- l = getMaxVariable' (getMaxVariable' b t) ls
    | otherwise = getMaxVariable' b ls

getFreeVariables :: DeBrTerm -> [Int]
getFreeVariables = getFreeVariables' 0

getFreeVariables' :: Int -> DeBrTerm -> [Int]
getFreeVariables' _ [] = []
getFreeVariables' c (t:ts)
    | Binder <- t = getFreeVariables' (c+1) ts
    | Variable i <- t
    , i > c
    = i : getFreeVariables' c ts
    | Inner l <- t = getFreeVariables' c l ++ getFreeVariables' c ts
    | otherwise = getFreeVariables' c ts

substitute :: DeBrTerm -> DeBrTerm -> DeBrTerm
substitute = substitute' 0

substitute' :: Int -> DeBrTerm -> DeBrTerm -> DeBrTerm
substitute' _ [] _ = []
substitute' c (t:ts) s
    | Substitute <- t = Inner (inc c (getFreeVariables s) s) : substitute' c ts s
    | Binder <- t = t : substitute' (c+1) ts s
    | Inner l <- t = Inner (substitute' c l s) : substitute' c ts s
    | otherwise = t : substitute' c ts s
    where
        inc :: Int -> [Int] -> DeBrTerm -> DeBrTerm
        inc _ _ [] = []
        inc i is (d:ds)
            | Variable x <- d
            , x `elem` is
            = Variable (x+i) : inc i is ds
            | Inner l <- d = Inner (inc i is l) : inc i is ds
            | otherwise = d : inc i is ds
