-- CptS 355 - Fall 2021 -- Homework1 - Haskell
-- Name: Andrew Todd
-- Collaborators:

module HW1
     where

-- Q1 everyOther

-- (Map and Filter)
-- everyOther xs = map fst (filter (odd . snd) (zip xs [1..]))

-- (Tail recursion)
everyOther [] = []
everyOther iL = helper iL []
                where helper [] xy = reverse xy
                      helper [x] xy = reverse (x:xy)
                      helper (x:y:xs) xy = helper xs (x:xy)

-- Q2(a) eliminateDuplicates (Tail recursion)
eliminateDuplicates [] = []
eliminateDuplicates iL = helper iL []
                         where helper [] xy = reverse xy
                               helper (x:xs) xy | (not(x `elem` xy)) = helper xs (x:xy)
                                                | otherwise = helper xs xy

-- Q2(b) matchingSeconds (Tail recursion)
matchingSeconds v [] = []
matchingSeconds v iL = helper v iL []
                       where helper v [] xy = reverse xy
                             helper v ((x, y):xs) xy | (x == v) = helper v xs (y:xy)
                                                     | otherwise = helper v xs xy

-- Q2(c) clusterCommon (Dirty explicit recursion)
clusterCommon [] = []
clusterCommon ((x, y):xs) = (x, matchingSeconds x ((x,y):xs)):clusterCommon (helper x xs)
                            where helper x [] = [] 
                                  helper x ((y, z):xs) | (x /= y) = (y, z):(helper x xs)
                                                       | otherwise = helper x xs

-- Q3 maxNumCases
getCases [] a = 0
getCases ((x, y):xs) a | (a == x) = y
                       | otherwise = getCases xs a

maxNumCases [] a = 0
maxNumCases ((x, xy):xs) a = getCases xy a `max` maxNumCases xs a

-- Q4 groupIntoLists
groupIntoLists [] = []
groupIntoLists iL = helper iL 1
                    where helper [] n = []
                          helper xs n = [take n xs] ++ helper (drop n xs) (n+1)

-- Q5 getSlice 
getSlice (a, b) [] = []
getSlice (a, b) (x:xs) | (x == a) = helper b xs
                       | (x /= a) = getSlice (a, b) xs 
                       | otherwise = []
                       where helper b [] = []
                             helper b (x:xs) | (x /= b) = x:(helper b xs)
                                             | (x == b) = []
                                             | otherwise = helper b xs

