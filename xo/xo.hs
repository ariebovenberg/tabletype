-- toy program for tic-tac-toe
-- TODO:
--   * check if moves do not replace eachother
--   * find out why some messages are printed twice
--   * add unittests
import Data.List
import qualified Data.Sequence
import Data.Sequence (Seq, fromList, chunksOf, update)
import Data.Foldable (toList)
import Data.Char (isDigit)

type Board = Seq

type Move = Board Place -> IO (Board Place)

data Outcome = XWins | OWins | Tie deriving (Show)

data Place = X | O | Empty deriving (Show, Eq)

play:: Board Place -> [Move] -> IO Outcome
play board (nextMove:moves) = do
  putStrLn $ "\nCurrent board:\n\n" ++ draw board ++ "\n"
  newBoard <- nextMove board
  case outcome newBoard of
    Tie -> play newBoard moves
    a -> do
      putStrLn $ "\nFinal board:\n\n" ++ draw newBoard
      return a
play board [] = do
  putStrLn $ "\nFinal board:\n\n" ++ draw board
  return Tie

player:: Place -> Move
player token board = do
  putStrLn $ "Place an " ++ show token ++ " token at this location (0-8):"
  char <- getChar
  if isDigit char
    then return $ update (read [char]) token board
    else player token board

main:: IO ()
main = do
    result <- play startBoard moves
    putStrLn $ "\nthe result is: " ++ show result
  where startBoard = Data.Sequence.replicate 9 Empty
        moves = take 9 $ cycle $ player <$> [X, O]

draw:: Board Place -> String
draw = intercalate "\n--+---+--\n" . map drawRow . toMatrix
       where drawRow = intercalate " | " . map drawPlace
             drawPlace Empty = " "
             drawPlace a = show a

toMatrix:: Board a -> [[a]]
toMatrix = toList . (fmap toList) . (chunksOf 3)

outcome:: Board Place -> Outcome
outcome b = if isWinner (positionsOf X) then XWins
            else if isWinner (positionsOf O) then OWins else Tie
            where positionsOf t = fmap (== t) b

isWinner:: Board Bool -> Bool
isWinner b = (isWinnerLeftToRight b) || (isWinnerLeftToRight $ rotate b)

rotate:: Board a -> Board a
rotate = fromList . concat . transpose . reverse . toMatrix

isWinnerLeftToRight:: Board Bool -> Bool
isWinnerLeftToRight b = any and (toMatrix b)  -- row winner
                || and [asList !! x | x <- [0, 4, 8]]  -- diagonal winner
                  where asList = toList b
