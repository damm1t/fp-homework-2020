import Test.Tasty

import TestError
import Test1_1
import Test1_2
import Test2_2
import Test3_1
import TestTreeStructure
import Test6

main :: IO()
main = parserTree >>= \ unitParserTests ->
  bracketSequenceTree >>= \ unitBracketTests ->
  numberParserTree  >>= \ unitNumTests ->
  maybeConcatTree >>= \ unitConcatTests ->
  natTestTree >>= \ unitNatTests -> 
      let allTests = testGroup "All tests" [ test1_1
                                           , unitNatTests
                                           , test1_2
                                           , test1_3
                                           , test2_2
                                           , unitConcatTests
                                           , unitParserTests
                                           , unitBracketTests
                                           , unitNumTests
                                           ] 
      in defaultMain allTests
