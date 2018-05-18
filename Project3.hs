{- Name: Zanyar Sherwa
   G#: G00841632
   Honestly starting to love haskell. Thank you!
-}

module Project3 where

-- optional: trace :: String -> a -> a, prints your string when
-- a's evaluation is forced.
import Debug.Trace
import Data.List
-- for writing files
import System.IO


------------------------------------------------------------------------
-- The following is needed for consistency between all our solutions.
------------------------------------------------------------------------

data RGB = RGB Int Int Int deriving (Show, Eq)
data Grid a = G [[a]] deriving (Show, Eq)

type Coord   = (Int,Int)
type Picture = Grid RGB
type Path    = [Coord]

type NodeEnergy = Int
type PathCost   = Int

type Packet = (Coord, RGB, NodeEnergy)  -- used much later
data Node = Node
        Coord             -- this location
        RGB               -- color info
        NodeEnergy        -- energy at this spot
        PathCost          -- cost of cheapest path from here to bottom.
        Node              -- ref to next node on cheapest path. Use No's when when we're at the bottom.
        (Node,Node,Node)  -- Three candidates we may connect to.
    | No   -- sometimes there is no next node and we use No as a placeholder.
    deriving (Show, Eq)
------------------------------------------------------------------------

width :: Grid a -> Int
width (G[]) = 0
width (G(x:xs)) = columns x

-- helper for the width function
columns [] = 0
columns (x:xs) = 1 + (columns xs)

height :: Grid a -> Int
height (G[]) = 0
height (G(x)) = rows x

-- helper for the height function
rows [] = 0
rows (x:xs) = 1 + (rows xs)

energyAt :: Grid RGB -> Coord -> NodeEnergy
energyAt (G ((grid))) (x, y) = (h_energy (G ((grid)) ) (x,y)) + (v_energy (G ((grid))) (x, y))

h_energy :: Grid RGB -> Coord -> Int
h_energy (G((grid))) (x, y) = ((getR (getRGBLeft (G ((grid))) x y))- (getR (getRGBRight (G ((grid))) x y)))^2 + ((getG (getRGBLeft (G ((grid))) x y))-( getG(getRGBRight (G ((grid))) x y)))^2 + ((getB (getRGBLeft (G ((grid))) x y))- (getB(getRGBRight (G ((grid))) x y)))^2

getR (RGB r g b) = r
getG (RGB r g b) = g
getB (RGB r g b) = b

v_energy (G((grid))) (x, y) = ((getR (getRGBAbove (G ((grid))) x y))- (getR (getRGBBelow (G ((grid))) x y)))^2 + ((getG (getRGBAbove (G ((grid))) x y))-( getG(getRGBBelow (G ((grid))) x y)))^2 + ((getB (getRGBAbove (G ((grid))) x y))- (getB(getRGBBelow (G ((grid))) x y)))^2


-- helper for energyAt
-- returns the left rgb value
getRGBLeft :: Grid RGB -> Int -> Int -> RGB
getRGBLeft (G((grid))) x y
  | y == 0 = (grid!!x)!!((width (G grid)) - 1)
  | otherwise = (grid!!x)!!(y-1)

-- helper for energyAt
-- returns the right rgb value
getRGBRight :: Grid RGB -> Int -> Int -> RGB
getRGBRight (G(grid)) x y
  | y >= ((width (G grid))-1)  = (grid!!x)!!0
  | otherwise = (grid!!x)!!(y+1)

-- helper for energyAt
-- returns the top rgb value
getRGBAbove :: Grid RGB -> Int -> Int -> RGB
getRGBAbove (G(grid)) x y
  | x == 0 = (grid!!( (height (G grid)) - 1 ))!!y
  | otherwise = (grid!!(x-1))!!y

-- helper for energyAt
-- returns the bottom rgb value
getRGBBelow :: Grid RGB -> Int -> Int -> RGB
getRGBBelow (G(grid)) x y
  | x >= ((height (G grid) )- 1) = (grid!!0)!!y
  | otherwise = (grid!!(x+1))!!y

{--
  this transposes it use it later
energies :: Grid RGB -> Grid NodeEnergy
energies (G(grid)) = (G [(energiesHelper (G(grid)) 0 y) | y <- [0..((width (G grid)) - 1)]])

energiesHelper :: Grid RGB -> Int -> Int -> [NodeEnergy]
energiesHelper (G (grid)) x y
    | x == ((height (G (grid)) ) - 1) = [(energyAt (G grid) (x,y))]
    | otherwise =  (energyAt (G grid) (x,y)): (energiesHelper (G grid) (x+1) y  )
  --}

energies :: Grid RGB -> Grid NodeEnergy
energies (G(grid)) = (G [(energiesHelper (G(grid)) x 0) | x <- [0..((height (G grid)) - 1)]])

energiesHelper :: Grid RGB -> Int -> Int -> [NodeEnergy]
energiesHelper (G (grid)) x y
    | y == ((width (G (grid)) ) - 1) = [(energyAt (G grid) (x,y))]
    | otherwise =  (energyAt (G grid) (x,y)): (energiesHelper (G grid) x (y+1)  )


findVerticalPath :: Grid RGB -> Path
findVerticalPath (G rgb) = (pathBuilder (cheapestPath (packetsToNodes (G (packetBuilder (G rgb) rgb 0 ((height (G rgb))) ))) ))

pathBuilder :: Node -> Path
pathBuilder No = []
pathBuilder (Node coord _ _ _ next _) = coord:(pathBuilder next)

cheapestPath :: Grid Node -> Node
cheapestPath (G (nodeHead:_)) = (cheapestPathHelper nodeHead)

cheapestPathHelper :: [Node] -> Node
cheapestPathHelper (nodeHead:[]) = nodeHead
cheapestPathHelper (nodeHead:nodeTail) = (cheapest nodeHead (cheapestPathHelper nodeTail))

packetBuilder :: Grid RGB -> [[RGB]] -> Int -> Int -> [[Packet]]
packetBuilder (G rgb) (rgbHead:rgbTail) row numRows
  | row == (numRows-1) = [[( (row,column), (rgbHead!!column), (energyAt (G rgb) (row, column)) )| column <- [0..((width (G(rgbHead:[])))-1)]]]
  | otherwise = [((row,column), rgbHead!!column, energyAt (G rgb) (row, column)) | column <- [0..((width (G(rgbHead:rgbTail)))-1)]]:(packetBuilder (G rgb) rgbTail (row+1) numRows)


buildNodeFromPacket :: Packet -> (Node, Node, Node) -> Node
buildNodeFromPacket (coord, rgb, energy) (nodeLeft, nodeDown, nodeRight) =  (Node coord rgb energy (energy + (cheapestNodeCost (cheapest (cheapest nodeLeft nodeDown) nodeRight)  )) (cheapest (cheapest nodeLeft nodeDown) nodeRight) (nodeLeft, nodeDown, nodeRight))

cheapest :: Node -> Node -> Node
cheapest No No = No
cheapest node1 No = node1
cheapest No node2 = node2
cheapest (Node a b c path1 d e) (Node f g h path2 i j)
  | path1 <= path2 = node1
  | otherwise = node2
  where node1 = (Node a b c path1 d e)
        node2 = (Node f g h path2 i j)


cheapestNodeCost (Node _ _ _ path _ _) = path
cheapestNodeCost No = 0

buildRowFromPackets :: [Packet] -> [(Node, Node, Node)] -> [Node]
buildRowFromPackets (p:ps) (n:ns)
    | ps == [] = [(buildNodeFromPacket p n)]
    | ns == [] = [(buildNodeFromPacket p n)]
    | otherwise = [(buildNodeFromPacket p n)]++(buildRowFromPackets ps ns)

packetsToNodes :: Grid Packet -> Grid Node
packetsToNodes (G packets) = G (reverse (packetsToNodesHelper (G (reverse packets))))

packetsToNodesHelper :: (Grid Packet) -> [[Node]]
packetsToNodesHelper (G []) = []
packetsToNodesHelper (G (packetHead:packetTail)) =  (f (G packetTail) (buildRowFromPackets packetHead [(No, No, No) | x <- [0..((width (G packetTail))-1) ] ]) 0)

f :: Grid Packet -> [Node] -> Int -> [[Node]]
f (G []) _ _ = []
f (G (packetHead:packetTail)) nodeList itr
  | itr == 0 = nodeList:(f (G(packetHead:packetTail)) nodeList 1)
  | otherwise = (buildRowFromPackets packetHead (nextGroups nodeList No)) : (f (G packetTail) (buildRowFromPackets packetHead (nextGroups nodeList No)) 1)

findHorizontalPath :: Grid RGB -> Path
findHorizontalPath (G rgb) = (invert (horizontalPathHelper rgb))

invert :: Path -> Path
invert (pathHead:[]) = [(invertHelper pathHead)]
invert (pathHead:pathTail) = (invertHelper pathHead):(invert pathTail)

invertHelper :: Coord -> Coord
invertHelper (x,y) = (y, x)

horizontalPathHelper :: [[RGB]] -> Path
horizontalPathHelper rgb = (findVerticalPath (G (transpose rgb)))

removeVerticalPath :: Grid RGB -> Path -> Grid RGB
removeVerticalPath (G rgb) path = (G (removeVerticalHelper rgb path 0)) --(packetsToNodes (G (packetBuilder (G rgb) rgb 0 ((height (G rgb))) )))

removeVerticalHelper :: [[RGB]] -> Path -> Int -> [[RGB]]
removeVerticalHelper (rgbHead:[]) (pathHead:pathTail) row = [[rgbHead!!column | column <- [0..((width (G(rgbHead:[]))) - 1)], not(column == y && row == x)]]
    where x = (getX pathHead)
          y = (getY pathHead)
removeVerticalHelper (rgbHead:rgbTail) (pathHead:path) row = [rgbHead!!column | column <- [0..((width (G(rgbHead:rgbTail))) - 1)], not(column == y && row == x)]:(removeVerticalHelper rgbTail path (row+1))
    where x = (getX pathHead)
          y = (getY pathHead)

getX (x,y) = x
getY (x, y) = y

removeHorizontalPath :: Grid RGB -> Path -> Grid RGB
removeHorizontalPath (G rgb) path = G (transpose (removeVerticalHelper (transpose rgb) (invert path) 0))

gridToFile :: Grid RGB -> FilePath -> IO()
gridToFile (G grid) filePath = writeFile filePath ((writeP3 (G grid)) ++ " " ++ format (G grid))

writeP3 :: Grid RGB -> [Char]
writeP3 (G rgb) = "P3" ++ " " ++ show (width (G rgb)) ++ " " ++ show (height (G rgb)) ++ " " ++ "255"

format :: Grid RGB -> [Char]
format (G []) = []
format (G (x:xs)) = formatHelper x ++ format (G xs)


formatHelper :: [RGB] -> [Char]
formatHelper [] = []
formatHelper (x:xs) = show (getR x) ++ " " ++ show (getG x) ++ " " ++ show (getB x) ++ " " ++ formatHelper xs

fileToGrid :: FilePath -> IO (Grid RGB)
fileToGrid = undefined

nextGroups :: [a] -> a -> [(a,a,a)]
nextGroups (x:xs) boundary
  | (width (G grid)  ) >= 2 = (nextGroupsHelper x xs boundary 0 (width (G grid)  ))
  | otherwise = [(boundary, x, boundary)]
  where grid = [x:xs]

nextGroupsHelper :: t -> [t] -> t -> Int -> Int -> [(t,t,t)]
nextGroupsHelper x xs boundary itr width
  | itr == 0 = [(boundary, x, xs!!0)]++(nextGroupsHelper x xs boundary (itr+1) width)   --[tail!!x | x <- [0..1]]++(nextGroupsHelper (head:tail) boundary (itr + 1))
  | (width-1) == itr  = [(x, xs!!0, boundary)]
  | otherwise = [(x, xs!!0, xs!!1)]++(nextGroupsHelper  (head xs) (tail xs)  boundary (itr+1) width)                       --[tail!!x | x <- [0..1]]++(nextGroupsHelper (head:tail) boundary (itr + 1))












