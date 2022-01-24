module HW2
     where

{- 1. groupbyNTail - 10%-}
groupbyNTail xs a = helper xs a []
                    where helper [] n ys = ys 
                          helper xs n ys = helper (drop n xs) n (ys ++ [take n xs])

-----------------------------------------------------------

{- 2.  elemAll and stopsAt  -  20% -}

{- (a)  elemAll - 10%-}
-- please don't include the myCatsLog list in your solution file. 
elemAll xs xy = (length $ filter (True==) $ map (\x -> elem x xy) xs) == (length xs)

{- (b) stopsAt - 10%-}
stopsAt xs ((x,y):ys) = map (fst) $ filter (\(x,y) -> elemAll xs y) ((x,y):ys)

-----------------------------------------------------------

{- 3. isBigger and applyRange - 25% -}
--define the Timestamp datatype
data Timestamp =  DATE (Int,Int,Int) |  DATETIME (Int,Int,Int,Int,Int) 
                  deriving (Show, Eq)

{- (a)  isBigger - 15% -}
dateHelp (x, y, z, _, _) = (x, y, z)
timeHelp (_, _, _, x, y) = (x, y)

isBigger (DATE a) (DATE b) = dateCheck a b
isBigger (DATE a) (DATETIME b) = dateCheck a (dateHelp b)
isBigger (DATETIME a) (DATE b) = dateCheck (dateHelp a) b
isBigger (DATETIME a) (DATETIME b) | (dateCheck (dateHelp a) (dateHelp b)) = True
                                   | (timeCheck (timeHelp a) (timeHelp b) && dateEqual (dateHelp a) (dateHelp b)) = True
                                   | otherwise = False

dateCheck (a, b, c) (d, e, f) | (c > f) = True
                              | (a > d) && (c == f) = True
                              | (b > e) && (a == d) && (c == f) = True
                              | otherwise = False

dateEqual (a,b,c) (d,e,f) | (a == d) && (b == e) && (c == f) = True
                          | otherwise = False

timeCheck (a, b) (c, d) | (a > c) = True
                        | (b > d) && (a == c) = True
                        | otherwise = False

{- (b) applyRange - 10% -}
applyRange (a, b) xs = filter (\x -> isBigger b x) $ filter (\x -> isBigger x a) xs 

-----------------------------------------------------------
{-4 - foldTree, createRTree, fastSearch  - 35%-}

--define Tree and RTree data types
data Tree a = LEAF a | NODE a (Tree a) (Tree a)
               deriving (Show,  Eq, Ord)

data RTree a = RLEAF a | RNODE a (a,a) (RTree a) (RTree a)
                    deriving (Show, Eq, Ord)

{- (a) foldTree - 8% -}
foldTree op (LEAF a) = a
foldTree op (NODE a b c) = op (a) $ op (foldTree op b) (foldTree op c) 

{- (b) createRTree - 12% -}
createRTree (LEAF a) = (RLEAF a)
createRTree (NODE a b c) = (RNODE a (foldTree min (NODE a b c), foldTree max (NODE a b c)) (createRTree b) (createRTree c)) 

{- (c) fastSearch - 15% -}
fastSearch (RLEAF a) f = ("leaf", a):[]
fastSearch (RNODE a (b,c) d e) f | (c >= f && f >= b) = ("node", a):(fastSearch d f) ++ (fastSearch e f)
                                 | otherwise = ("node", a):[]

-------------------------------------------------------------------

{- Tree Examples 5% -}
-- include your tree examples in the test file. 
{-Testing your tree functions - 5%-}


