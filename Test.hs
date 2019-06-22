import Dijkstra

step :: (Int,Int) -> [(Float,(Int,Int))]
step (x,y) = [(if dx==0 || dy==0 then 1.0 else 1.4142135623730951,
    (x+dx,y+dy)) | dx<-[-1,0,1], dy<-[-1,0,1], dx/=0 || dy/=0 ]

main :: IO ()
main = do
    print $ dijkstra step (0,0) (15,4)
    return ()
