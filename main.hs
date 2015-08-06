import Control.Monad
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

moveSnake :: [(Int, Int)] -> Char -> Int -> Int -> [(Int, Int)]
moveSnake ((a,b):xs) 'u' i j= (check (a - 1, b) i j xs):(a,b):init xs
moveSnake ((a,b):xs) 'd' i j= (check (a + 1, b) i j xs):(a,b):init xs
moveSnake ((a,b):xs) 'l' i j= (check (a, b - 1) i j xs):(a,b):init xs
moveSnake ((a,b):xs) 'r' i j= (check (a, b + 1) i j xs):(a,b):init xs
moveSnake xs _ _ _= xs

growSnake :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
growSnake xs i j = (i,j):xs

putSnake :: [(Int, Int)] -> [[Char]] -> [[Char]]
putSnake [] g = g
putSnake ((i,j):xs) g = setAtIndex (putSnake xs g) i j '#'

playGame :: [(Int, Int)] -> Int -> Int -> IO ()
playGame snake i j = do
    (input:rest) <- getLine
    let newSnake = moveSnake snake input i j 
    putStrLn $ showGrid $ putSnake newSnake $ genGrid i j
    playGame newSnake i j

main = playGame [(5,5),(5,6), (5,7)] 10 10
