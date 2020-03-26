import Test.Tasty

import TestError
import Test1_1
import Test1_2
import Test2_2
import Test3_1
import TestTreeStructure

main :: IO()
main = maybeConcatTree >>= \ unitTests1 -> natTestTree >>= \ unitTests2 ->
  let allTests = testGroup " All tests" [test1_1, unitTests2, test1_2, test1_3, test2_2, unitTests1] in defaultMain allTests
