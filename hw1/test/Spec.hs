import Test.Tasty
--import Test.Tasty.QuickCheck as QC
--import Test.Tasty.SmallCheck as SC

import Task1_1Test
import Task1_2Test

main :: IO()
main = defaultMain $ testGroup "tests for hw 1" [task1_1Test, task1_2Test]
