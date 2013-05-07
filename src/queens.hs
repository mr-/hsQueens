import Data.Tree (Tree(..))
import Data.List (union, (\\), intersect) 
--unlines, intersperse, unfoldr)
--import Data.Foldable (maximumBy)
import Data.Tree.Zipper 
-- (childAt, fromTree, tree, parent, Full, TreePos)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.Console.Haskeline (outputStrLn, getInputLine, runInputT, defaultSettings, InputT)

type Pos = (Integer, Integer) 
type Board = [Pos] 


data Command = Auto Integer | Up | Go Integer deriving (Read)

main :: IO ()
main = foo 9
 
foo :: Integer -> IO ()
foo n = runInputT defaultSettings (loop $ fromTree $ prunedTree n)
  where added old new = head $ new \\ old 
        showOpt options curBoard = outputStrLn $ foldr (\(a,b) y -> (show a) ++ ":" ++ (show b) ++ "  " ++ y) ""   
                  (zip [0..] $ map (added curBoard) options) 

        loop :: TreePos Full Board -> InputT IO ()
        loop treePos = do
            let options = map rootLabel $ subForest $ tree treePos
                curBoard = rootLabel $ tree treePos
            outputStrLn $ prettyBoard n $ rootLabel $ tree treePos 
            showOpt options curBoard
            tP <- handleCommand treePos
            loop tP


interpret :: TreePos Full Board -> Command -> Either String (TreePos Full Board)
interpret treePos Up | (isRoot treePos)  = Left "We are at the top"
interpret treePos Up  = Right $ fromJust $ parent treePos 

interpret treePos (Go n) = case child of 
                              Nothing -> Left "No such choice"
                              Just x  -> Right x
  where child = childAt (fromInteger n) treePos

interpret treePos (Auto n)   = case null found of
                                  True  -> Left "Nothing found"
                                  False -> Right $ head found 
  where found = findPosBelow (\t -> length (rootLabel t) >= (fromInteger n)) treePos


handleCommand :: TreePos Full Board -> InputT IO (TreePos Full Board)
handleCommand  treePos = do  
  inp <- getInputLine "> "
  case readMaybe (fromJust inp) of
    Nothing   -> do outputStrLn "Invalid Command (Have Go N, Up and Auto N so far)"
                    handleCommand treePos
    Just x    -> case (interpret treePos x) of
                    Left s  -> do outputStrLn s
                                  handleCommand treePos
                    Right t -> return t

findPosBelow :: (Tree Board -> Bool) -> TreePos Full Board -> [TreePos Full Board]
findPosBelow f pos | hasChildren pos = l ++ concatMap (findPosBelow f) childs
  where childs = unfoldr' next fc
        fc = fromJust $ firstChild pos
        l = if f (tree pos) then [pos] else [] 

findPosBelow f pos | f (tree pos) = [pos]
findPosBelow _ _ = [] 

unfoldr'      :: (a -> Maybe a) -> a -> [a]
unfoldr' f b  =
  case f b of
   Just a   -> a : unfoldr' f a
   Nothing  -> []


prunedTree :: Integer -> Tree Board
prunedTree n = pruneTree n $ buildTree n (Node [] [])



positions :: Integer -> [Pos]
positions size = [ (x, y) | x <- [1..size], y <- [1..size]]


buildTree :: Integer -> Tree Board -> Tree Board
buildTree size (Node node _) = Node node descendents
    where   descendents = [buildTree size (Node board []) | board <- newBoards]
            newBoards = map (:node) uniquePositions
            uniquePositions = positions size \\ node

pruneTree :: Integer -> Tree Board -> Tree Board
pruneTree size (Node node subTrees) = Node node prunedSubs
    where prunedSubs = map (pruneTree size) goodDescendents
          goodDescendents = filter (isGood node) subTrees
          isGood n subTree = staysConsistent size n 
                            (head $ rootLabel subTree \\ n) 
                            --can assume that new label arises by adding one node

staysConsistent :: Integer -> Board -> Pos -> Bool
staysConsistent size board piece = null $ intersect activeNeighs board 
    where activeNeighs = activeNeighborhood size piece

activeNeighborhood :: Integer -> Pos -> [Pos]
activeNeighborhood size (x,y) = active
    where line1 = [(x + d, y ) | d <- [(-size)..size]
                                , x + d > 0, x + d <= size]
          line2 = [(x , y + d) | d <- [(-size)..size]
                                , y + d > 0, y + d <= size]
          diag1 = [(x + d , y + d ) | d <- [(-size)..size]
                                , x + d > 0, x + d <= size
                                , y + d > 0, y + d <= size]
          diag2 = [(x + d , y - d ) | d <- [(-size)..size]
                                , x + d > 0, x + d <= size
                                , y - d > 0, y - d <= size]
          active = line1 `union` line2 `union` diag1 `union` diag2


prettyBoard :: Integer -> Board -> String
prettyBoard size board = unlines $ map concat [ [cell x y | x <- [1..size]] | y <- [1..size] ]
    where cell x y = if (x,y) `elem` board then "Q" else "_"