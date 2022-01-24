-- CptS 355 - Lab 1 (Haskell) - Spring 2021
-- Name: Andrew Todd 
-- Collaborated with: Allison Stansberry 

module Lab1
    where

-- 1. Insert
insert 0 it [] = it:[]
insert n it [] = []
insert 1 it xs = it:xs
insert n it xs | (n > length xs) = xs
               | otherwise = take n xs ++ [it] ++ take (length xs) (drop n xs)
--insert n it (x:xs) = x:(insert (n-1) it xs)

-- 2. insertEvery
insertEvery 0 it [] = it:[]
insertEvery 1 it xs = it:xs
insertEvery n it [] = []
insertEvery n it xs | (n > length xs) = xs
                    | otherwise = take n xs ++ [it] ++ insertEvery n it (drop n xs)

-- 3. getSales
-- getSales :: (Num p, Eq t) => t -> [(t, p)] -> p
getSales a [] = 0
getSales a ((x, y):xs) | (a == x) = y + getSales a xs
                       | otherwise = getSales a xs

-- 4. sumSales
sumSales a b [] = 0
sumSales a b ((x, y):xs) | (a == x) = sum((getSales b y):[sumSales a b xs])
                         | otherwise = sumSales a b xs

-- 5. split
split a [] = []
split a (x:xs) = let b = (splitHelp a (x:xs))
                 in b:(split a (drop (length b) xs))
                  where splitHelp a [] = []
                        splitHelp a (x:xs) | (x /= a) = x:(splitHelp a xs)
                                           | otherwise = []
