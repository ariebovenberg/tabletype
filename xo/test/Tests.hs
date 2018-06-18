import qualified Data.Sequence
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)
import           Xo               (isWinner)

main :: IO ()
main = defaultMain unitTests

unitTests =
  testGroup
    "unit tests"
    [nobodyWinsEmptyBoard]


nobodyWinsEmptyBoard =
  testCase "X is diagonal winner" $ assertEqual [] False $ isWinner (Data.Sequence.replicate 9 False)
