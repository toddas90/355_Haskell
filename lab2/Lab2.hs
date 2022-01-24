-- CptS 355 - Lab 2 (Haskell) - Fall 2021
-- Name: Andrew Todd 
-- Collaborated with: Allison Stansberry 

module Lab2
     where

-- Eliminate Duplicates from HW1
eliminateDuplicates [] = []
eliminateDuplicates iL = helper iL []
                         where helper [] xy = reverse xy
                               helper (x:xs) xy | (not(x `elem` xy)) = helper xs (x:xy)
                                                | otherwise = helper xs xy

-- 1
{- (a) merge2 -}
merge2 [] [] = []
merge2 (x:xs) [] = x:(merge2 xs [])
merge2 [] (y:ys) = y:(merge2 [] ys)
merge2 (x:xs) (y:ys) = x:y:(merge2 xs ys) 

{- (b) merge2Tail -}
merge2Tail l1 l2 = helper l1 l2 []
                   where helper [] [] rL = reverse rL
                         helper (x:xs) [] rL = helper xs [] (x:rL)
                         helper [] (y:ys) rL = helper [] ys (y:rL) 
                         helper (x:xs) (y:ys) rL = helper xs ys (y:x:rL)

{- (c) mergeN -}
mergeN iL = foldl (merge2Tail) [] iL

-- 2
{- (a) count -}
count a [] = 0
count a iL = length $ filter (==a) iL

{- (b) histogram  -}
histogram [] = []
histogram iL = eliminateDuplicates $ map (\x -> (x, count x iL)) iL

-- 3                
{- (a) concatAll -}
concatAll iL = foldr (++) "" $ foldr (++) [] iL


{- (b) concat2Either -}               
data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)

--concat2Either iL = AString (foldr (++) "" $ map (show) iL)

-- 4      
{-  concat2Str -}               




data Op = Add | Sub | Mul | Pow
          deriving (Show, Read, Eq)

evaluate:: Op -> Int -> Int -> Int
evaluate Add x y =  x+y
evaluate Sub   x y =  x-y
evaluate Mul x y =  x*y
evaluate Pow x y = x^y

data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
                  deriving (Show, Read, Eq)

-- 5 
{- evaluateTree -}
evaluateTree :: ExprTree Int -> Int
evaluateTree (ELEAF a) = a
evaluateTree (ENODE op childl childr) =  evaluate op (evaluateTree childl) (evaluateTree childr)

-- 6
{- printInfix -}



--7
{- createRTree -}
data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
                     deriving (Show, Read, Eq)

createRTree (ELEAF a) = RLEAF a
createRTree (ENODE op childl childr) = RNODE (evaluateTree (ENODE op childl childr)) (createRTree childl) (createRTree childr)





