import Data.Tree (Tree(..))
import Data.List ((\\), sortBy)
import Control.Monad.Trans.Class (lift)
import Data.Tree.Zipper ( childAt, fromTree, root, isRoot, 
                          parent, tree, firstChild, next, hasChildren, 
                          Full, TreePos )
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import Text.Read (readMaybe)
import System.Console.Haskeline (outputStrLn, getInputLine, runInputT, 
                                 defaultSettings, InputT)
import System.Environment (getArgs)
import Data.Time (getZonedTime, formatTime)
import System.Locale (defaultTimeLocale)
import Parser

type Piece = (Int, Int)
type Board = [Piece] 

-- type Expression = [Command]
-- data Command = Auto Int | Up | Top | Go Int deriving (Read, Show)


main :: IO ()
main = do args <- getArgs
          let x = (listToMaybe args >>= readMaybe) :: Maybe Int
          runQueens $ fromMaybe 8 x
 
runQueens :: Int -> IO ()
runQueens n = runInputT defaultSettings (loop $ Just $ fromTree $ searchTree n)
  where 
        showOpt options  = outputStrLn $ foldr (\(a,b) y -> show a ++ ":" ++ show b ++ "  " ++ y) ""   
                  (zip [(0::Int)..] $ map latest options) 

        loop :: Maybe (TreePos Full Board) -> InputT IO ()
        loop Nothing = outputStrLn "Bye bye"
        loop (Just treePos) = do
            let options = map rootLabel $ subForest $ tree treePos
            outputStrLn $ prettyBoard n $ rootLabel $ tree treePos 
            showOpt options 
            tP <- handleCommand treePos
            loop tP

latest :: Board -> Piece
latest = head

getTime :: IO String
getTime = do
          zt <- getZonedTime
          return $ formatTime defaultTimeLocale "%H:%M:%S" zt


interpretExpression :: Expression -> TreePos Full Board -> Either String (TreePos Full Board)
interpretExpression [] _ = error "Internal Error in interpretExpression"
interpretExpression [cmd] treePos = interpretCommand treePos cmd 
interpretExpression (x:xs) treePos = interpretCommand treePos x >>= interpretExpression xs

interpretCommand :: TreePos Full Board -> Command -> Either String (TreePos Full Board)
interpretCommand treePos (Des n) = Right $ fromTree $ toTree $ head $ findBelow f $ cutTreeBy f $ tree treePos
  where f node = length node >=  n
        toTree node = Node node [] 
interpretCommand treePos Top = Right $ root treePos
interpretCommand treePos Up | isRoot treePos  = Left "We are at the top"
interpretCommand treePos Up  = Right $ fromJust $ parent treePos 

interpretCommand treePos (Go n) = case child of 
                              Nothing -> Left "No such choice"
                              Just x  -> Right x
  where child = childAt  n treePos

interpretCommand treePos (Auto n)   = case null found of
                                  True  -> Left "Nothing found"
                                  False -> Right $ head found 
  where found = findPosBelow (\t -> length (rootLabel t) >= n) treePos

handleCommand :: TreePos Full Board -> InputT IO (Maybe (TreePos Full Board))
handleCommand  treePos = do
  time <- lift  getTime  
  inp <- getInputLine (time ++ "> ")
  case inp of
    Nothing -> return Nothing 
    Just text  -> case readExpr text >>= \cmd -> interpretExpression cmd treePos of
                          Left s  -> do outputStrLn s
                                        handleCommand treePos
                          Right t -> return (Just t)
{-                          
handleCommand :: TreePos Full Board -> InputT IO (Maybe (TreePos Full Board))
handleCommand  treePos = do
  time <- lift  getTime  
  inp <- getInputLine (time ++ "> ")
  case readMaybe <$> inp of
    Nothing        -> return Nothing 
    Just Nothing   -> do outputStrLn "Invalid Command (Have Go N, Up, Top and Auto N so far)"
                         handleCommand treePos
    Just (Just x)    -> case interpretExpression x treePos of
                          Left s  -> do outputStrLn s
                                        handleCommand treePos
                          Right t -> return (Just t)
-}
  
findPosBelow :: (Tree Board -> Bool) -> TreePos Full Board -> [TreePos Full Board]
findPosBelow f pos | hasChildren pos = [pos | f (tree pos)] ++ rest
  where rest = concatMap (findPosBelow f) childs
        childs = fc : unfoldr' next fc
        fc = fromJust $ firstChild pos
findPosBelow f pos | f (tree pos) = [pos]
findPosBelow _ _ = [] 

findBelow :: (Board -> Bool) -> Tree Board -> [Board]
findBelow p (Node node subTrees) = [node | p node] ++ concatMap (findBelow p) subTrees


unfoldr'      :: (a -> Maybe a) -> a -> [a]
unfoldr' f b  =
  case f b of
   Just a   -> a : unfoldr' f a
   Nothing  -> []


searchTree :: Int -> Tree Board
searchTree n = heuristics $ pruneTree $ buildTree n (Node [] [])


positions :: Int -> [Piece]
positions size = [ (x, y) | x <- [1..size], y <- [1..size]]


buildTree :: Int -> Tree Board -> Tree Board
buildTree size (Node node _) = Node node descendents
    where   descendents = [buildTree size (Node board []) | board <- newBoards]
            newBoards = map (:node) uniquePositions
            uniquePositions = positions size \\ node

--this gets us from 15sec to < 1sec for the 8 queens problem
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


cutTreeBy :: (Board -> Bool) -> Tree Board -> Tree Board
cutTreeBy p (Node node subTrees) = Node node trimmedSubs
  where trimmedSubs = map (cutTreeBy p) goodSubs 
        goodSubs = filter (has p) subTrees

has :: (Board -> Bool) -> Tree Board -> Bool
has p (Node node subTrees) = p node || any (has p) subTrees

staysConsistent :: Board -> Piece -> Bool          
staysConsistent board new = not $ any (isUnconsistentWith new) board
    where  isUnconsistentWith (x,y) (x',y') = 
            x == x' || y == y' || (x-x') == (y-y') || (x-x') == -(y-y') 


prettyBoard :: Int -> Board -> String
prettyBoard size board = unlines $ map concat [ [cell x y | x <- [1..size]] | y <- [1..size] ]
    where cell x y = if (x,y) `elem` board then "♛" else "."
