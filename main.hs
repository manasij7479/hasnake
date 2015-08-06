myshowList [] = ""
myshowList (x:xs) = '|':x:'|':myshowList(xs)

showGrid [] = ""
showGrid (x:xs) = myshowList x ++ "\n" ++ showGrid xs

genGrid m n = replicate m $ replicate n '.'

setAtListIndex [] _ _ = []
setAtListIndex (x:xs) i c = if i == 0 then c:xs else x:setAtListIndex xs (i-1) c

getAtListIndex [] _ = error "Out of bounds list access"
getAtListIndex (x:xs) i = if i == 0 then x else getAtListIndex xs (i-1) 


setAtIndex grid i j c = setAtListIndex grid i (setAtListIndex (getAtListIndex grid i) j c)  
getAtIndex grid i j = getAtListIndex (getAtListIndex grid i) j 

check :: (Int, Int) -> Int -> Int -> [(Int, Int)] -> (Int, Int)
check (px,py) i j xs = if not(elem (px, py) xs)
                      && (px >= 0) && (px < i)
                      && (py >= 0) && (py < j)
                    then (px, py) 
                    else error "Game Over"

data Dir = UpDir | DownDir | LeftDir | RightDir

charToDir :: Char -> Dir
charToDir 'u' = UpDir
charToDir 'd' = DownDir
charToDir 'l' = LeftDir
charToDir 'r' = RightDir
charToDir _ = error "Invalid Input"

nextPos :: (Int, Int) -> Dir -> (Int, Int)
nextPos (a, b) UpDir     = (a - 1, b)
nextPos (a, b) DownDir   = (a + 1, b)
nextPos (a, b) LeftDir   = (a, b - 1)
nextPos (a, b) RightDir  = (a, b + 1)

growSnake :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
growSnake xs i j = (i,j):xs

sumCoords :: [(Int, Int)] -> Int
sumCoords [] = 0
sumCoords ((x,y):xs) = x + y + sumCoords xs

arbiter :: [(Int, Int)] -> Int -> Int -> Dir -> Int -> Int -> (Int, Int)
arbiter xs x y d i j = let (nX, nY) = ((x*71+y*43) `mod` i,(x*29+y*97) `mod` j) in
                        if elem (nX, nY) xs
                        then
                            arbiter xs nX nY d i j
                        else (nX, nY)


moveSnake :: [(Int, Int)] -> Dir -> Int -> Int -> Int -> Int -> ([(Int, Int)], Int, Int)
moveSnake ((a,b):xs) d x y i j= 
    let np = nextPos (a,b) d in
        if np == (x,y)
        then
            let (newX, newY) = arbiter ((a,b):xs) x y d i j in
            (growSnake ((a,b):xs) x y, newX, newY) 
        else
            ((check np i j xs):(a,b):init xs, x, y)

putSnake :: [(Int, Int)] -> [[Char]] -> [[Char]]
putSnake [] g = g
putSnake ((i,j):xs) g = setAtIndex (putSnake xs g) i j '#'

putFood :: Int -> Int -> [[Char]] -> [[Char]] 
putFood x y grid = setAtIndex grid x y '$'

playGame :: [(Int, Int)] -> Int -> Int -> Int -> Int -> IO ()
playGame snake x y i j = do
    (input:rest) <- getLine
    let (newSnake, newX, newY) = moveSnake snake (charToDir input) x y i j
    putStrLn $ showGrid $ putFood newX newY $ putSnake newSnake $ genGrid i j
    playGame newSnake newX newY i j

main = playGame [(5,5),(5,6), (5,7)] (1) (1) 10 10
