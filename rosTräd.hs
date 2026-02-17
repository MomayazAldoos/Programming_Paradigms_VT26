-- Löv är en nod utan barn 

data RoseTree a = N a [RoseTree a] 
    deriving (Eq, Show)

-- om vi en lista [0[1, 2, 3 [4, 5]]]
-- det är sammma som en träd 
--     0
--     /|\
--    1 2 3
--       /\
--      4 5


t1 :: RoseTree Int
t1 = N 0 [N 1 [],N 2 [],N 3 [N 4 [],N 5 []]]

leaves :: RoseTree a -> [a]
leaves (N x []) = [x]
leaves (N _ xs) = concat (map leaves xs)



-- funktion som returnerar alla värderna i ett träd

flatten :: RoseTree a -> [a]
flatten (N x xs) = x : concat (map flatten xs)


-- funktion som adderar ett element till alla siffrorna i trädet. 
mapRoseTree :: (a -> b) -> RoseTree a -> RoseTree b
mapRoseTree f (N x xs) =
    let 
        newX = f x
        child = map (mapRoseTree f) xs
    in
        N newX child

--- Matrices into lists
data Matrix a = Empty | Cons a [a] [a] (Matrix a)
  deriving (Eq,Show)

matrixToList :: Matrix a -> [[a]]
matrixToList Empty = []
matrixToList (Cons a r c m) =
    (a:r) : zipWith (:) c (matrixToList m ++ repeat [])


-- Lists into matrices

listToMatrix :: [[a]] -> Matrix a
listToMatrix [] = Empty
listToMatrix ([]:_) = Empty -- if the first row is empty, then we have an empty matrix
listToMatrix (row0:restRows)=
    let 
        a = head row0
        r = tail row0
        c = map head restRows
        m = listToMatrix (map tail restRows)
    in
        Cons a r c m


--- function for all values in a matrix
mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix f Empty = Empty
mapMatrix f (Cons a r c m) =
    Cons (f a) (map f r) (map f c) (mapMatrix f m)

-- Negate all elements in a matrix
negMatrix :: (Num a) => Matrix a -> Matrix a
negMatrix = mapMatrix negate

-- Multiply all elements in a matrix by a scalar
scalarMul :: (Num a) => a -> Matrix a -> Matrix a
scalarMul k = mapMatrix (* k)

-- d) Addition for matrices
addMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
addMatrix Empty Empty = Empty
addMatrix Empty m = m
addMatrix m Empty = m
addMatrix (Cons a1 r1 c1 m1) (Cons a2 r2 c2 m2) =
    Cons (a1 + a2) (zipWith (+) r1 r2) (zipWith (+) c1 c2) (addMatrix m1 m2)

-- Subtraction for matrices (using addMatrix and negMatrix)
subMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
subMatrix m n = addMatrix m (negMatrix n)

-- Multiplication of a column vector and a row vector to produce a matrix
mulColRow :: Num a => [a] -> [a] -> Matrix a
mulColRow [] _ = Empty
mulColRow _ [] = Empty
mulColRow (c:cs) row =
    let firstRow = map (c *) row
        a = head firstRow
        r = tail firstRow
        cRest = map (* head row) cs
        subMatrix = mulColRow cs (tail row)
    in Cons a r cRest subMatrix


-- Tests for matrices:
m1 :: Matrix Integer
m1 = Cons 1 [2] [3] (Cons 4 [] [] Empty)

m2 :: Matrix Integer
m2 = Cons 42 [1,-2] [3,-9] m1

m3 :: Matrix Double
m3 = Cons 2.0 [1.0,3.2,4.1] [3.2,1.4,1.0]
         (Cons 9.9 [5.323,56.4123] [15.323,7.4993]
               (Cons 0 [0] [0] (Cons 1 [] [] Empty)))

m4 :: Matrix Integer
m4 = Cons 1 [2,3,4] [] Empty

m5 :: Matrix Integer
m5 = Cons 1 [] [2,3,4] Empty

m6 :: Matrix Integer
m6 = Cons 1 [2] [3,5] (Cons 4 [] [6] Empty)


main :: IO ()
main = do 
    putStrLn ("Löv i t1: " ++ show (leaves t1))
    putStrLn ("Alla värderna i t1: " ++ show (flatten t1))
    putStrLn ("Adderar 1 till alla värderna i t1: " ++ show (mapRoseTree (*10) t1))
    --- Tests for matrices
    putStrLn ("Matriser empty: " ++ show (matrixToList (Empty :: Matrix Int)))
    putStrLn ("Matriser m1: " ++ show (matrixToList (m1)))
    putStrLn ("Matriser m2: " ++ show (matrixToList (m2)))
    putStrLn ("Matriser m3: " ++ show (matrixToList (m3)))
    putStrLn ("Matriser m4: " ++ show (matrixToList (m4)))
    putStrLn ("Matriser m5: " ++ show (matrixToList (m5)))
    putStrLn ("Matriser m6: " ++ show (matrixToList (m6)))
    
    -- Tests for listToMatrix
    putStrLn "\n--- listToMatrix tester ---"
    putStrLn ("listToMatrix ([] :: [[Int]]): " ++ show (listToMatrix ([] :: [[Int]])))
    putStrLn ("listToMatrix [[1,2],[3,4]]: " ++ show (listToMatrix [[1,2],[3,4]]))
    putStrLn ("listToMatrix [[42,1,-2],[3,1,2],[-9,3,4]]: " ++ show (listToMatrix [[42,1,-2],[3,1,2],[-9,3,4]]))
    putStrLn ("listToMatrix [[1,2,3,4]]: " ++ show (listToMatrix [[1,2,3,4]]))
    putStrLn ("listToMatrix [[1],[2],[3],[4]]: " ++ show (listToMatrix [[1],[2],[3],[4]]))
    putStrLn ("listToMatrix [[1,2],[3,4],[5,6]]: " ++ show (listToMatrix [[1,2],[3,4],[5,6]]))
    
    -- Tests for mapMatrix, negMatrix, and scalarMul
    putStrLn "\n--- mapMatrix tester ---"
    putStrLn ("mapMatrix (\\x -> x ^ 2) m6: " ++ show (mapMatrix (\x -> x ^ 2) m6))
    
    putStrLn "\n--- negMatrix tester ---"
    putStrLn ("negMatrix m1: " ++ show (negMatrix m1))
    putStrLn ("negMatrix m2: " ++ show (negMatrix m2))
    putStrLn ("negMatrix m3: " ++ show (negMatrix m3))
    
    putStrLn "\n--- scalarMul tester ---"
    putStrLn ("scalarMul 0 m1: " ++ show (scalarMul 0 m1))
    putStrLn ("scalarMul 2 m1: " ++ show (scalarMul 2 m1))
    putStrLn ("scalarMul 3 m2: " ++ show (scalarMul 3 m2))
    putStrLn ("scalarMul (-1.2) m3: " ++ show (scalarMul (-1.2) m3))
    
    -- Tests for addMatrix and subMatrix
    putStrLn "\n--- addMatrix tester ---"
    putStrLn ("addMatrix m1 m1: " ++ show (addMatrix m1 m1))
    putStrLn ("addMatrix m2 m2: " ++ show (addMatrix m2 m2))
    putStrLn ("addMatrix m5 m5: " ++ show (addMatrix m5 m5))
    putStrLn ("addMatrix m6 m6: " ++ show (addMatrix m6 m6))
    
    putStrLn "\n--- subMatrix tester ---"
    putStrLn ("subMatrix m2 m2: " ++ show (subMatrix m2 m2))
    
    -- Tests for mulColRow
    putStrLn "\n--- mulColRow tester ---"
    putStrLn ("mulColRow [1,2] [3,4]: " ++ show (mulColRow [1,2] [3,4]))
    putStrLn ("mulColRow [1..5] [6..10]: " ++ show (mulColRow [1..5] [6..10]))