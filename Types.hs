import Data.List

data Point = Point Int Int 
			|Point2 Int Int Int
			deriving (Show)

data Nonomino = Points { p0 :: Point
						,p1 :: Point
						,p2 :: Point
						,p3 :: Point
						,p4 :: Point
						,p5 :: Point
						,p6 :: Point
						,p7 :: Point
						,p8 :: Point} 
						deriving (Show)

print_ x =  putStr $ show x ++ "\t" 
table xxs 
    | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
    | otherwise = mapM_ printRow xxs 
        where printRow xs =  mapM_ print_ xs >> putStrLn "" 


getX (Point x _) = x
getX (Point2 x _ _) = x

getY (Point _ y) = y
getY (Point2 _ y _) = y

getvalue (Point _ _ ) = 0
getvalue (Point2 _ _ x)= x

getCor :: Point -> [Int]
getCor (Point x y ) = [x,y]
getCor (Point2 x y v) = [x,y] 

positionValid :: Int -> Int -> Bool
positionValid x y = x >= 0 && y>=0 && x<9 && y<9

checkRow :: [[Int]] -> Bool
checkRow [] = True
checkRow (x:xs) = (length values == length uniqValues) && checkRow xs
                    where
                    values = filter (>0) x
                    uniqValues = nub values

checkColumn :: [[Int]] -> Bool
checkColumn matrix = checkRow (transpose matrix)

checkSudoku :: [[Int]] -> Bool
checkSudoku matrix = checkRow matrix && checkColumn matrix


checkSudokuNom :: [Nonomino] -> Bool
checkSudokuNom  [] = True

checkSudokuNom  (x:nom) = temp && checkSudokuNom nom  
                        where
                        points = getPoints x
                        values = map getvalue points
                        temp = (length values) == (length $ nub values)


checkSudokuAll :: [[Int]] -> [Nonomino] -> Bool
checkSudokuAll matrix nom = checkSudoku matrix && checkSudokuNom nom

freeFirstElement :: [[Int]] -> (Int,Int)
freeFirstElement sudoku = head [
    (x,y) |
    (t,x) <- zip sudoku [0..],
    (e,y) <- zip t [0..],
    e == (-1)
    ]
--Insertar un punto--
insertPoint :: Point -> Int -> Int -> Point
insertPoint (Point x y) i j = Point (x+i) (y+j)
insertPoint (Point2 x y v) i j = Point2 (x+i) (y+j) v
--Insertar un nonomino--
insertNonomino :: Nonomino -> Int -> Int -> Nonomino
insertNonomino (Points p1 p2 p3 p4 p5 p6 p7 p8 p9) i j = 
													Points mp1 mp2 mp3 mp4 mp5 mp6 mp7 mp8 mp9
                                                   	where
                                                   	mp1 = insertPoint p1 i j
                                                   	mp2 = insertPoint p2 i j
                                                   	mp3 = insertPoint p3 i j
                                                   	mp4 = insertPoint p4 i j
                                                   	mp5 = insertPoint p5 i j
                                                   	mp6 = insertPoint p6 i j
                                                   	mp7 = insertPoint p7 i j
                                                   	mp8 = insertPoint p8 i j
                                                   	mp9 = insertPoint p9 i j


getPoints :: Nonomino -> [Point]
getPoints (Points p0 p1 p2 p3 p4 p5 p6 p7 p8) = [p0,p1,p2,p3,p4,p5,p6,p7,p8]

getOnlyPoint :: Point -> [Int]
getOnlyPoint (Point x y) = [x,y]
getOnlyPoint (Point2 x y _) = [x,y]

-- Devolver la matrix modificada --
replace :: [a] -> Int -> a -> [a]
replace list i element = take i list ++ [element] ++ drop (i + 1) list

replaceMatrix :: [[Int]] -> Point -> [[Int]]
replaceMatrix matrix (Point i j) = replace matrix i replacedRow
                               where
                               row = matrix !! i
                               replacedRow = replace row j 0
replaceMatrix matrix (Point2 i j v) = replace matrix i replacedRow
                               where
                               row = matrix !! i
                               replacedRow = replace row j v
---------------------------------------------------

curriedReplaceMatrix :: [[Int]] -> Point -> [[Int]]
curriedReplaceMatrix = replaceMatrix

setNonomino :: [[Int]] -> Nonomino -> Int -> Int -> [[Int]]
setNonomino matrix nonomino i j = foldl curriedReplaceMatrix matrix points
								where
									points = getPoints (insertNonomino nonomino i j)

validNonomino :: [[Int]] -> Nonomino -> Int -> Int -> Bool
validNonomino matrix nm i j = all (\[x,y] -> positionValid x y && (((matrix !! x) !! y) == (-1)) ) coordPoint
                                where
                                points = getPoints (insertNonomino nm i j)
                                coordPoint = map getOnlyPoint points


validAllNonominosPermutationsPosible :: [[Int]] -> [Nonomino] -> Bool
validAllNonominosPermutationsPosible matrix [] = checkSudoku matrix
validAllNonominosPermutationsPosible matrix (x:xs)
                                | validNonomino matrix x i j = 
                                    let cMatrix = setNonomino matrix x i j
                                    in validAllNonominosPermutationsPosible cMatrix xs
                                | otherwise = False
                                where
                                    free = freeFirstElement matrix
                                    i = fst free
                                    j = snd free

allNonominosPermutationsPosible :: [[Int]] -> [Nonomino] -> [[Nonomino]]
allNonominosPermutationsPosible matrix nom = filter (validAllNonominosPermutationsPosible matrix) nmPermutations
                                    where nmPermutations = permutations nom

nonominoReplace :: Nonomino -> (Int,Int) -> Int -> Nonomino
nonominoReplace nonominoe (i,j) v = resultnonomino
                                where
                                squares = getPoints nonominoe
                                filtered = [s | s <- squares , not(getCor s == [i,j])]

                                resultnonomino = Points (filtered !! 0) (filtered !! 1) (filtered !! 2) (filtered !! 3) (filtered !! 4) (filtered !! 5) (filtered !! 6) (filtered !! 7)(filtered !! 8)

nonominoToMatrix :: [[Int]] -> [Nonomino] -> [[Int]]
nonominoToMatrix matrix [] = matrix
nonominoToMatrix matrix (x:noms) = nonominoToMatrix newMatrix noms
								where
									newMatrix = setNonomino matrix x i j
									elem = freeFirstElement matrix
									i = fst elem
									j = fst elem



-- calcPossible :: [[Int]] -> [Nonomino] -> (Int,Int) -> [Int]
-- calcPossible sudoku nom (x,y) | sudoku !! x !! y > 0 = []
--                               | otherwise = getRow sudoku x `intersect` oposite (getColumn sudoku y) `intersect` getPiece sudoku nom x y

-- constRowP :: [[Int]] -> [Nonomino] -> Int -> [[Int]]
-- constRowP sudoku nom x = [ calcPossible sudoku nom (x,y) | y <- [0..8]]

-- constMatrixPosible :: [[Int]] -> [Nonomino] -> [[[Int]]]
-- constMatrixPosible sudoku nom = [ constRowP sudoku nom x | x <- [0..8]]

-- dropPosibleList :: [[[Int]]] -> Int -> Int -> Int -> [[[Int]]]
-- dropPosibleList matrixposible i j element = dropAllPosible
--                                             where
--                                               droprow = dropPosibleRow matrixposible i element
--                                               dropcolumn = dropPosibleColumn matrixposible j element
--                                               dropAllPosible = 
-- --
-- sudokuSolutions :: [[Int]] -> [[[Int]]] -> Int -> Int -> [[[Int]]]

-- sudokuSolutions matrix matrixposible _ 9 = [matrix]
-- sudokuSolutions matrix matrixposible i 9 = sudokuSolutions matrix matrixposible (i+1) 0
-- sudokuSolutions matrix matrixposible i j  | posvalue > 0 = sudokuSolutions matrix matrixposible i (j+1)
--                                           | otherwise = concatMap (\(newMatrix , newmatrixposible) -> sudokuSolutions newMatrix newmatrixposible i (j+1)) combinationTuple
--                                           where
--                                            row = matrix !! i
--                                            posvalue = (matrix !! i) !! j
--                                            posiblevalue = (matrix !! i ) !! j
--                                            newMatrixTemp =  [take i matrix ++ [take j row ++ [e] ++ drop (j + 1) row] ++ drop (i + 1) matrix | e <- posiblevalue]
--                                            newmatrixposibleTemp = [ dropPosibleList matrixposible i j element | element <- posiblevalue]
--                                            combinationTuple = zip newMatrixTemp newmatrixposibleTemp
--


 -- solution :: [[Nonominos]] -> [[[Int]]]

 -- solution [x] = sudokuSolutions matrix matrixposible 0 0
 --              where
 --              initialmatrix = constMatrix -1
 --              matrix = nonominoToMatrix initialmatrix x
              
 --              matrixposible = constMatrixPosible matrix x

--Creacion de los nonominos y la matrix vacia-----

constMatrix :: Int->[[Int]]
constMatrix (n)  = (replicate 9 (replicate 9 n))

matrix = constMatrix (-1)

nonomino1 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
            (Point2 0 3 1)  (Point 0 4) (Point 1 1)
            (Point 1 2)         (Point 1 3) (Point 2 2)
nonomino2 = Points (Point2 0 0 2)  (Point 0 1)         (Point2 0 2 3)
            (Point 0 3)         (Point 1 (-1))      (Point 1 0)
            (Point 1 1)         (Point2 1 2 9)  (Point 1 3)
nonomino3 = Points (Point2 0 0 6)  (Point 1 0)         (Point 1 1)
            (Point 1 3)         (Point2 2 0 8)  (Point 2 1)
            (Point 2 2)         (Point 2 3)         (Point2 3 2 4)
nonomino4 = Points (Point 0 0)         (Point 0 1) (Point 0 2)
            (Point2 0 3 8)  (Point 0 4) (Point 1 3)
            (Point 1 4)         (Point 2 3) (Point 2 4)
nonomino5 = Points (Point2 0 0 7)      (Point2 1 (-1) 5)   (Point 1 0)
            (Point2 2 (-2) 2)   (Point 2 (-1))          (Point2 3 (-2) 9)
            (Point2 4 (-2) 1)   (Point 5 (-3))          (Point 5 (-2))
nonomino6 = Points (Point2 0 0 4)  (Point 0 1)    (Point2 1 0 6)
            (Point 1 1)         (Point 2 (-1)) (Point 2 0)
            (Point 2 1)         (Point 3 1)    (Point 4 1)
nonomino7 = Points (Point 0 0)         (Point 0 1)         (Point 1 0)
            (Point 1 1)         (Point2 2 0 3)  (Point 2 1)
            (Point2 3 0 4)  (Point 3 1)         (Point 4 0)
nonomino8 = Points (Point 0 0)     (Point 0 1)         (Point 1 0)
            (Point 1 1)     (Point 2 0)         (Point 2 1)
            (Point 3 (-1))  (Point2 3 0 7)  (Point 3 1)
nonomino9 = Points (Point2 0 0 8)  (Point 0 1)         (Point2 0 2 5)
            (Point2 1 0 7)  (Point 1 1)         (Point2 1 2 9)
            (Point2 2 0 3)  (Point2 2 1 6)  (Point 2 2)

noms = [nonomino1 , nonomino2 , nonomino3 , nonomino4,nonomino5,nonomino6,nonomino7,nonomino8,nonomino9]

permutationsNomValid = allNonominosPermutationsPosible matrix noms

initialmatrix = nonominoToMatrix matrix (head (permutationsNomValid))