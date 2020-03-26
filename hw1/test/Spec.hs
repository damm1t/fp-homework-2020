import Test.Tasty

import TestError
import Task1_1Test
import Task1_2Test

main :: IO()
main = natTestTree >>= \ unitTests ->
  let allTests = testGroup " All tests" [task1_1Test, unitTests, task1_2Test] in defaultMain allTests
