import Dijkstra
import qualified Data.Matrix as M
import System.Environment (getArgs)

levelRead :: String -> IO ((Int,Int),M.Matrix Char)
levelRead fname = do
    content <- readFile fname
    let lins = filter (not . null) $ lines content
    let [px,py,_] = map read $ words (lins !! 1)
    let lists = map (filter (/=' ')) $ drop 2 lins
    let matrix = M.fromLists lists
    return ((py+1,px+1),matrix)

levelFind :: M.Matrix Char -> Char -> [(Int,Int)]
levelFind ma c = [(y,x) | x<-[1..M.ncols ma], y<-[1..M.nrows ma],
    let v = ma M.! (y,x), v == c]

step :: (Int,Int) -> [(Float,(Int,Int))]
step (y,x) = [
    (1.0,(y+1,x)),
    (1.0,(y,x+1)),
    (1.0,(y-1,x)),
    (1.0,(y,x-1)),
    (1.4142135623730951,(y-1,x-1)),
    (1.4142135623730951,(y-1,x+1)),
    (1.4142135623730951,(y+1,x-1)),
    (1.4142135623730951,(y+1,x+1))]

pathCost :: [(Float,(Int,Int))] -> Float
pathCost pa = if null pa
    then 1.0/0.0
    else sum $ map fst pa

movement :: M.Matrix Char -> (Int,Int) -> [(Float,(Int,Int))]
movement ma (py,px)
    | ma M.! (py,px) == 'x' = moves'++tpoints
    | otherwise             = moves'
    where
    moves = step (py,px)
    moves' = filter (\(f,(y,x)) -> y>0 && x>0 && y<=M.nrows ma && x<=M.ncols ma
        && ma M.! (y,x) /= '#') moves
    tpoints = map (\p -> (1.5,p)) $ levelFind ma 'x'

nearestPath :: M.Matrix Char -> (Int,Int) -> [(Float,(Int,Int))]
nearestPath ma ini
    | null demons = []
    | otherwise   = minp
    where
    demons = (levelFind ma 'b') ++ (levelFind ma 'B')
    paths = map (dijkstra (movement ma) ini) demons
    (_,minp) = minimum $ map (\p -> (pathCost p,p)) paths

printStep :: (Float,(Int,Int)) -> String
printStep (c,(y,x)) = show (x-1,y-1) ++ " w/cost "++show c

main :: IO ()
main = do
    args <- getArgs
    (p,level) <- levelRead (args !! 0)
    let path = nearestPath level p
    if null path then do
        putStrLn "There is no path."
    else do
        putStrLn ("Path cost: "++show (pathCost path))
        mapM_ putStrLn (map printStep path)
