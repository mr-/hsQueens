import Data.Tree (Tree(..))
import Data.List ((\\), sortBy)
import Data.Maybe (listToMaybe, fromMaybe, fromJust)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Data.Foldable (find)



type Piece = (Int, Int)
type Board = [Piece] 


main :: IO ()
main = do args <- getArgs
          let x = (listToMaybe args >>= readMaybe) :: Maybe Int
          runQueens $ fromMaybe 8 x

runQueens :: Int -> IO ()
runQueens n =
    putStrLn $ prettyBoard n $ fromJust $ find f $ cleanedTree n
      where f node = length node >= n


cleanedTree :: Int -> Tree Board
cleanedTree n = cutTreeBy f  $ heuristics $ pruneTree $ buildTree n (Node [] [])
    where f node = length node >= n


positions :: Int -> [Piece]
positions size = [ (x, y) | x <- [1..size], y <- [1..size]]


buildTree :: Int -> Tree Board -> Tree Board
buildTree size (Node node _) = Node node descendents
    where   descendents = [buildTree size (Node board []) | board <- newBoards]
            newBoards = map (:node) uniquePositions
            uniquePositions = positions size \\ node


heuristics :: Tree Board -> Tree Board
heuristics (Node node subTrees) = Node node (sortBy childCount subTrees)
  where 
    childCount (Node _ x) (Node _ y) = length x `compare` length y 

pruneTree :: Tree Board -> Tree Board
pruneTree  (Node node subTrees) = Node node prunedSubs
    where prunedSubs = map pruneTree goodDescendents
          goodDescendents = filter (isGood node) subTrees
          newPiece t = head $ rootLabel t
          isGood board newBoard = staysConsistent board (newPiece newBoard)

staysConsistent :: Board -> Piece -> Bool          
staysConsistent board new = not $ any (isUnconsistentWith new) board
    where  isUnconsistentWith (x,y) (x',y') = 
            x == x' || y == y' || (x-x') == (y-y') || (x-x') == -(y-y') 

cutTreeBy :: (Board -> Bool) -> Tree Board -> Tree Board
cutTreeBy p (Node node subTrees) = Node node trimmedSubs
  where trimmedSubs = map (cutTreeBy p) goodSubs 
        goodSubs = filter (has p) subTrees

has :: (Board -> Bool) -> Tree Board -> Bool
has p (Node node subTrees) = p node || any (has p) subTrees


prettyBoard :: Int -> Board -> String
prettyBoard size board = unlines $ map concat [ [cell x y | x <- [1..size]] | y <- [1..size] ]
    where cell x y = if (x,y) `elem` board then "♛" else "."
