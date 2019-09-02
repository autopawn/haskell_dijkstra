import Dijkstra

import qualified Data.Matrix as M

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

main :: IO ()
main = do
    print (dijkstra step (0,0) (15,4))
