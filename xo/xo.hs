-- toy program for tic-tac-toe
import Data.List
import qualified Data.Sequence
import Data.Sequence (Seq, fromList, chunksOf, update)
import Data.Foldable (toList)

type Board = Seq
type Move = Board Place -> IO (Board Place)
data Outcome = XWins | OWins | Tie deriving (Show, Eq)
data Place = X | O | Empty deriving (Show, Eq)


emptyBoard:: Board Place
emptyBoard = Data.Sequence.replicate 9 Empty

play:: Board Place -> [Move] -> IO Outcome
play board (nextMove:moves) = do
  putStrLn $ draw board
  putStrLn "\nnext move..."
  newBoard <- nextMove board
  play newBoard moves
play board [] = do
  putStrLn "\nfinal board:"
  putStrLn $ draw board
  putStrLn "\nthe result:"
  return $ outcome board

demoMoves:: [Move]
demoMoves = [autoMove 5 X, autoMove 4 O,
             autoMove 1 X, autoMove 6 O,
             autoMove 8 X, autoMove 2 O]
            where autoMove index token = return . update index token

main:: IO ()
main = do
    final <- play emptyBoard demoMoves
    putStrLn $ show final

draw:: Board Place -> String
draw = intercalate "\n--+---+--\n" . map drawRow . toMatrix
       where drawRow = intercalate " | " . map drawPlace
             drawPlace Empty = " "
             drawPlace a = show a

toMatrix:: Board a -> [[a]]
toMatrix = toList . (fmap toList) . (chunksOf 3)

rotate:: Board a -> Board a
rotate = fromList . concat . transpose . reverse . toMatrix

outcome:: Board Place -> Outcome
outcome b = if isWinner (positionsOf X) then XWins
            else if isWinner (positionsOf O) then OWins else Tie
            where positionsOf t = fmap (== t) b

isWinner:: Board Bool -> Bool
isWinner b = (isWinnerLeftToRight b) || (isWinnerLeftToRight $ rotate b)

isWinnerLeftToRight:: Board Bool -> Bool
isWinnerLeftToRight b = any and (toMatrix b)  -- row winner
                || and [asList !! x | x <- [0, 4, 8]]  -- diagonal winner
                   where asList = toList b
