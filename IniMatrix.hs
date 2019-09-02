import qualified Data.Matrix as M

main = do
  let mat = M.fromLists [['a','b','c'],['d','e','f']]
  print (mat M.! (1,1))
  print (mat M.! (2,1))
  print (mat M.! (1,2))

