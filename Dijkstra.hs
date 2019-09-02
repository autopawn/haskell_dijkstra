module Dijkstra (dijkstra) where

import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MSet

{- Expands a tree of states Dijkstra's way. Keeping an "open list" of states
and a Map where the keys are the closed states, the values are the costs
and parents of the move that allowed to reach it. -}
dijkstraTree :: (Ord s) =>
    (s -> [(Float,s)]) ->                -- Function that retrieves the neighbors of a node, and the move cost.
    s ->                                 -- Goal state where to stop
    MSet.MultiSet (Float,s,(Float,s)) -> -- Used as priority queue of open nodes (total cost,node,(move cost,parent))
    Map.Map s (Float,s) ->               -- Map of closed nodes, the value stored is a pair cost,parent node
    Map.Map s (Float,s)                  -- Final map of all closed nodes
dijkstraTree moves goal open clos
    -- No more open nodes
    | null open       = clos
    -- Next node already explored
    | Map.member st clos  = dijkstraTree moves goal open' clos
    -- Next node is the goal
    | st == goal          = clos'
    -- Regular move
    | otherwise       = dijkstraTree moves goal open'' clos'
    where
    -- find next node to be explored
    ((tc,st,mv),open') = MSet.deleteFindMin open
    -- closed nodes adding st
    vecs = [(tc+c,s,(c,st)) | (c,s) <- moves st]
    -- closed nodes adding st
    clos' = Map.insert st mv clos
    -- open nodes, adding neighbors of st
    open'' = foldr MSet.insert open' vecs

{- This function retrieves a path from end to ini from a tree
It assumes the path exists. -}
pathTo :: (Ord s) => Map.Map s (Float,s) -> s -> s -> [(Float,s)]
pathTo clos ini end = let
    (c,prev) = clos Map.! end
    in if ini == end then [(0,ini)] else (c,end):(pathTo clos ini prev)

{- Retrieves the minimum total cost list of movements (cost,next state)
from he initial state s0 to the goal.
The list starts with (0,s0).
If there is no path, a null list is retrieved. -}
dijkstra :: (Ord s) =>
    (s -> [(Float,s)]) -> s -> s -> [(Float,s)]
dijkstra moves s0 goal
    -- The goal was reached on the tree
    | Map.member goal tree = reverse (pathTo tree s0 goal)
    -- The goal wasn't reached
    | otherwise            = []
    where
    tree = dijkstraTree moves goal (MSet.singleton (0,s0,(0,s0))) Map.empty
