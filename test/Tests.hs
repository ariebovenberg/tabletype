import qualified Data.Sequence
import           Test.Tasty
import           Test.Tasty.HUnit (assertEqual, testCase)
import           Xo               (isWinner)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [nobodyWinsEmptyBoard, winningDiagonally]


nobodyWinsEmptyBoard :: TestTree
nobodyWinsEmptyBoard =
  testCase "nobody wins an empty board" $ assertEqual [] False
    $ isWinner (Data.Sequence.replicate 9 False)

winningDiagonally :: TestTree
winningDiagonally =
  testCase "diagonal win" $ assertEqual [] True
    $ isWinner (Data.Sequence.fromList [False, False, True,
                                        False, True, False,
                                        True, False, False])
