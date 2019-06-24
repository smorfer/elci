module Main where

import System.IO
import System.Environment
-- TO-DO Use HashMap from the unordered containers package
import qualified Data.Map as Map

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    fhandle <- openFile file ReadMode
    string <- hGetContents fhandle
    let statements = fmap trim ( filter (not . null) (lines string))
    interpret statements Map.empty

data Statement = Assignment | Statement | Error String deriving (Show)
data DeBruijn = Binder | Variable Int | Symbol String | Substitute | Inner DeBrTerm deriving (Read)
type DeBrTerm = [DeBruijn]

instance Show DeBruijn where
    show Binder     = "\x03BB"
    show (Variable i) = show i
    show Substitute = "\x2588"
    show (Inner t) = " (" ++ showTerm t ++ ")"
    show (Symbol s) = "{"++s++"}"

showTerm :: DeBrTerm -> String
showTerm = foldr ((++) . show) ""

putTerm :: DeBrTerm -> IO ()
putTerm ts = putStrLn $ showTerm ts


findOccurrence :: DeBrTerm -> DeBrTerm
findOccurrence [] = []
findOccurrence (x:xs)
    | Binder <- x = findOccurrence' xs 1
    | otherwise = x:xs

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
    | Substitute <- t
    , [d] <- s
    , Variable i <- d
    = Variable (i+c) : substitute' c ts s
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

betaReduce :: DeBrTerm -> DeBrTerm
betaReduce [] = []
betaReduce [t]
    | Inner d <- t = betaReduce d
    | otherwise = [t]
betaReduce (t:ts)
    | Binder <- t = t : betaReduce ts
    | Variable _ <- t = t : betaReduce' ts
    | Inner d <- t
    , Inner f <- head ts
    = if null $ tail ts
        then betaReduce $ substitute (findOccurrence (betaReduce d)) (betaReduce f)
        else betaReduce (Inner (betaReduce $ substitute (findOccurrence (betaReduce d)) (betaReduce f)) : tail ts)
    | Inner d <- t
    , Variable i <- head ts
    = if null $ tail ts
        then betaReduce $ substitute (findOccurrence (betaReduce d)) [Variable i]
        else betaReduce (Inner (betaReduce $ substitute (findOccurrence (betaReduce d)) [Variable i]) : tail ts)
    | otherwise = error "Symbols/Substitutes not valid in beta reduction"

betaReduce' :: DeBrTerm -> DeBrTerm
betaReduce' [] = []
betaReduce' (t:ts)
    | Inner d <- t = Inner (betaReduce d) : betaReduce' ts
    | otherwise = t : betaReduce' ts

simplify :: DeBrTerm -> DeBrTerm
simplify [] = []
simplify (t:ts)
    | Inner l <- t
    , [Inner x] <- l
    = Inner (simplify x) : simplify ts
    | otherwise = t : simplify ts


lambdaCurry :: String -> String
lambdaCurry [] = []
lambdaCurry (x:xs)
    | '|' <- x = currytoDot xs
    | otherwise = x : lambdaCurry xs
        where
            currytoDot :: String -> String
            currytoDot [] = []
            currytoDot (s:ss)
                | s `elem` ['a'..'z'] = '|' : s : currytoDot ss
                | otherwise = s : lambdaCurry ss

lambdaUncurry :: String -> String
lambdaUncurry [] = []
lambdaUncurry (x:xs)
    | '|' <- x = x : uncurrytoDot xs
    | otherwise = x : lambdaUncurry xs
        where
            uncurrytoDot :: String -> String
            uncurrytoDot [] = []
            uncurrytoDot (s:ss)
                | s `elem` ['a'..'z'] = s : uncurrytoDot ss
                | '|' <- s = uncurrytoDot ss
                | otherwise = s : lambdaUncurry ss



determineStatement :: String -> Statement
determineStatement [] = Error "Parse Error: Empty String"
determineStatement (x:xs)
    | x == ' ' = determineStatement xs
    | x `elem` ['A'..'Z'] = determineStatement' Assignment (x:xs)
    | '|' == x
    = determineStatement' Statement (x:xs)
    | otherwise = Error ("Parse error: " ++ [x] ++ " is not a valid beginning of a Statement or Assignment ")

determineStatement' :: Statement -> String -> Statement
determineStatement' s [] = s
determineStatement' _ [x]
    = Error ("Parse error: " ++ [x] ++ " is not a valid beginning of a Statement or Assignment")
determineStatement' s (x:y:xs)
    | Statement <- s
    = isStatement (x:y:xs)
    | x `elem` ['A'..'Z']
    , y == ' '
    , Assignment <- s
    = isAssignment xs
    | x `elem` ['A'..'Z']
    , y == '='
    , Assignment <- s
    , Statement <- isStatement xs
    = Assignment
    | x `elem` ['A'..'Z']
    , y `elem` ['A'..'Z']
    , not $ null xs
    = determineStatement' s (y:xs)
    | x `elem` ['A'..'Z']
    , y `elem` ['A'..'Z']
    , null xs
    = Statement
    | otherwise = Error ("Parse Error: " ++ (x:y:xs) ++ " can't be parsed as any Statement")
    where
        isStatement :: String -> Statement
        isStatement [] = Error "Parse Error: Empty String"
        isStatement [a]
            | a `elem` ['a'..'z']
            || a `elem` ['A'..'Z']
            || a `elem` [' ','|','.','(',')']
            = Statement
            | otherwise = Error ("Parse Error: " ++ [a] ++ " not valid in Statement")
        isStatement (a:as)
            | a `elem` ['a'..'z']
            || a `elem` ['A'..'Z']
            || a `elem` [' ','|','.','(',')']
            = isStatement as
            | otherwise = Error ("Parse Error:" ++ [a] ++ " not valid in Statement")

        isAssignment :: String -> Statement
        isAssignment [] = Error "Parse Error: Empty String"
        isAssignment (a:as)
            | a == '='
            , Statement <- isStatement as
            = Assignment
            | a == ' ' = isAssignment as
            | otherwise = isStatement as



trim :: String -> String
trim xs = reverse (trim' (reverse (trim' xs)))
    where
        trim' :: String -> String
        trim' [] = error "trim: Empty String"
        trim' (a:as)
            | a == ' ' = trim' as
            | otherwise = a:as

getAssignment :: String -> (String,String)
getAssignment xs = (getKey xs, trim $ getTerm xs)
    where
        getKey :: String -> String
        getKey [] = error "getKey: Empty String"
        getKey (a:as)
            | a == ' '
            || a == '='
            = []
            | otherwise = a : getKey as
        getTerm :: String -> String
        getTerm [] = error "getTerm: Empty String"
        getTerm (a:as)
            | a == '=' = as
            | otherwise = getTerm as

getDebruijnIndex :: Char -> [Char] -> Int
getDebruijnIndex = getDebruijnIndex' 1

getDebruijnIndex' :: Int -> Char -> [Char] -> Int
getDebruijnIndex' i _ [] = i
getDebruijnIndex' i c (x:xs)
    | c == x = i
    | otherwise = getDebruijnIndex' (i+1) c xs

evaluateStatement :: String -> DeBrTerm
evaluateStatement = evaluateStatement' []

evaluateStatement' :: [Char] -> String -> DeBrTerm
evaluateStatement' _ [] = []
evaluateStatement' ds [x]
    | x == ' ' = []
    | x `elem` ['a'..'z'] = [Variable (getDebruijnIndex x ds)]
    | otherwise = error ("Parse Error: " ++ [x] ++ " not valid in Statement")
evaluateStatement' ds (x:y:xs)
    | x == ' '
    || x == '.'
    = evaluateStatement' ds (y:xs)
    | x == '|'
    , y `elem` ['a'..'z']
    = Binder : evaluateStatement' (y:ds) xs
    | x == '('
    , y /= ')'
    = Inner (evaluateStatement' ds (getInner (y:xs))) : evaluateStatement' ds (getAfter (y:xs))
    | x `elem` ['a'..'z'] = Variable (getDebruijnIndex x ds) : evaluateStatement' ds (y:xs)
    | x `elem` ['A'..'Z'] = Symbol (getSymbol (x:y:xs)) : evaluateStatement' ds (getAfterSymbol (x:y:xs))
    | otherwise = error ("Parse Error: " ++ [x,y] ++ " not valid in Statement")

    where
        getInner :: String -> String
        getInner [] = error "Parse Error: Paranthesis empty String"
        getInner (a:as)
            | a == ')' = []
            | a == '(' = a : getInner' 1 as
            | otherwise = a : getInner as

        getInner' :: Int -> String -> String
        getInner' i []
            | i == 0 = []
            | otherwise = error "Parse Error: Paranthesis Mismatch"
        getInner' i (a:as)
            | i == 0 = []
            | a == '(' = a : getInner' (i+1) as
            | a == ')' = a : getInner' (i-1) as
            | otherwise = a : getInner' i as

        getAfter :: String -> String
        getAfter [] = error "Parse Error: Paranthesis empty String"
        getAfter (a:as)
            | a == ')' = as
            | a == '(' = getAfter' 1 as
            | otherwise = getAfter as

        getAfter' :: Int -> String -> String
        getAfter' i []
            | i == 0 = []
            | otherwise = error "Parse Error: Paranthesis Mismatch"
        getAfter' i (a:as)
            | i == 0 = as
            | a == '(' = getAfter' (i+1) as
            | a == ')' = getAfter' (i-1) as
            | otherwise = getAfter' i as

        getSymbol :: String -> String
        getSymbol [] = []
        getSymbol (a:as)
            | a `elem` ['A'..'Z'] = a : getSymbol as
            | otherwise = []

        getAfterSymbol :: String -> String
        getAfterSymbol [] = []
        getAfterSymbol (a:as)
            | a `elem` ['A'..'Z'] = getAfterSymbol as
            | otherwise = (a:as)

substituteSymbols :: DeBrTerm -> Map.Map String DeBrTerm -> DeBrTerm
substituteSymbols [] _ = []
substituteSymbols (t:ts) m
    | Symbol x <- t = Inner (unsafeLookup x m) : substituteSymbols ts m
    | Inner x <- t = Inner (substituteSymbols x m) : substituteSymbols ts m
    | otherwise = t : substituteSymbols ts m
    where
        unsafeLookup :: String -> Map.Map String DeBrTerm -> DeBrTerm
        unsafeLookup s am
            | Just d <- Map.lookup s am = d
            | otherwise = error ("Parse Error: Symbol " ++ s ++ " not defined")

indextoVariable :: Int -> [Char] -> Char
indextoVariable _ [] = '#'
indextoVariable i (x:xs)
    | i == 1 = x
    | otherwise = indextoVariable (i-1) xs

debrTermtoLambdaString :: DeBrTerm -> String
debrTermtoLambdaString t = snd $ debrTermtoLambdaString' ['a'..'z'] [] t

debrTermtoLambdaString' :: [Char] -> [Char] -> DeBrTerm -> ([Char],String)
debrTermtoLambdaString' vs _ [] = (vs, "")
debrTermtoLambdaString' [] _ _ = error "Support for more than 26 Symbols is not implemented yet"
debrTermtoLambdaString' (v:vs) us (t:ts)
    | Binder <- t
    , Variable _ <- head ts
    = let
        (xs,ys) = debrTermtoLambdaString' vs (v:us) ts
        in (xs, '|':v:'.':ys)
    | Binder <- t
    , Inner _ <- head ts
    = let
        (xs,ys) = debrTermtoLambdaString' vs (v:us) ts
        in (xs, '|':v:'.':ys)
    | Binder <- t = let
        (xs,ys) = debrTermtoLambdaString' vs (v:us) ts
        in (xs, '|':v:ys)
    | Variable i <- t
    , c <- indextoVariable i us
    , c == '#'
     = let
        (xs,ys) = debrTermtoLambdaString' vs us ts
        in (xs, v:ys)
    | Variable i <- t = let
        (xs,ys) = debrTermtoLambdaString' (v:vs) us ts
        in (xs, indextoVariable i us:ys)
    | Inner ds <- t = let
        (xs,ys) = debrTermtoLambdaString' (v:vs) us ds
        (as, bs) = debrTermtoLambdaString' xs us ts
        in (as, "(" ++ ys ++ ")" ++ bs)
    | otherwise = error "Symbols/Substitutes are not valid in lambda string"

interpret :: [String] -> Map.Map String DeBrTerm -> IO ()
interpret [] _ = putStrLn "Finished"
interpret (l:ls) m
    | Assignment <- determineStatement l
    = do
        let (a,t) = getAssignment l
        let st = simplify $ betaReduce $ substituteSymbols (evaluateStatement (lambdaCurry t)) m
        let nm  = Map.insert a st m
        interpret ls nm
    | Statement <- determineStatement l
    = do
        let st = simplify $ betaReduce $ substituteSymbols (evaluateStatement (lambdaCurry l)) m
        putStrLn (l ++ " -> " ++ lambdaUncurry  (debrTermtoLambdaString st))
        interpret ls m
    | Error s <- determineStatement l = error (s ++ "\n In the Expression:\n" ++ l)
    | otherwise = error ("Couldn't determine Statement:\n" ++ l)
