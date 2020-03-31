module Test6 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, describe, it,
  testSpec, shouldBe)

import Task6_1
import Task6_2
import Task6_3
import Data.Char (isLower, toUpper, isDigit, isUpper)
import Test.Tasty.QuickCheck (property)
import Control.Applicative ((<|>))

parserTree :: IO TestTree
parserTree = testSpec "Parser tests" parserSpec

checkOk :: String -> Bool
checkOk s = runParser ok s == Just ((), s)

checkStream :: String -> String -> Bool
checkStream x xs = runParser (stream x) (x ++ xs) == Just (x, xs)

parserSpec :: Spec
parserSpec = describe "Stream tests" $ do
  it "functor" $
    runParser (toUpper <$> satisfy isLower) "xyz"
    `shouldBe`
    Just ('X', "yz")

  it "functor fail" $
    runParser (toUpper <$> satisfy isLower) "Xyz"
    `shouldBe`
    Nothing

  it "applicative" $
    runParser ((++) <$> stream "abra" <*> stream "cadabra") "abracadabra"
    `shouldBe`
    Just ("abracadabra", "")

  it "first appicative fail" $
    runParser ((++) <$> stream "abla" <*> stream "cadabra") "abracadabra"
    `shouldBe`
    Nothing

  it "second appicative fail" $
    runParser ((++) <$> stream "abra" <*> stream "cadabla") "abracadabra"
    `shouldBe`
    Nothing

  it "alternative" $
    runParser (element 'y' <|> element 'x') "xyz"
    `shouldBe`
    Just ('x',"yz")

  it "alternative fail" $
    runParser (element 'y' <|> element 'x') "zxy"
    `shouldBe`
    Nothing
    
  it "monad" $
      runParser (satisfy isUpper >>= element) "XXY"
      `shouldBe`
      Just ('X',"Y")
      
  it "monad fail" $
    runParser (satisfy isUpper >>= element) "XYZ"
    `shouldBe`
    Nothing
      
  it "ok test with string" $ property checkOk

  it "parse eof" $
    runParser eof "xyz"
    `shouldBe`
    Nothing

  it "parse eof with empty input" $
    runParser eof ""
    `shouldBe`
    Just ((), "")

  it "satisfy test with element" $
    runParser (satisfy isDigit) "1mpala"
    `shouldBe`
    Just ('1', "mpala")

  it "satisfy test without element match" $
    runParser (satisfy isDigit) "s1mple"
    `shouldBe`
    Nothing

  it "satisfy test with empty" $
    runParser (satisfy isDigit) ""
    `shouldBe`
    Nothing

  it "signle element" $
    runParser (element 'x') "xyz"
    `shouldBe`
    Just ('x', "yz")

  it "element without match" $
    runParser (element 'z') "xyz"
    `shouldBe`
    Nothing

  it "empty string element" $
    runParser (element 'x') ""
    `shouldBe`
    Nothing

  it "stream test with string" $
    runParser (stream "abra") "abracadabra"
    `shouldBe`
    Just ("abra", "cadabra")

  it "stream test without match" $
    runParser (stream "xyz") "wtfIsThis"
    `shouldBe`
    Nothing

  it "stream test with empty string" $
    runParser (stream "who") ""
    `shouldBe`
    Nothing

  it "stream test" $
    property checkStream

bracketSequenceTree :: IO TestTree
bracketSequenceTree = testSpec "Task 6-3 Brackets" bracketSeqSpec

seqToStr :: Maybe (BracketSeq, String) -> Maybe (String, String)
seqToStr = fmap (first show)

bracketSeqSpec :: Spec
bracketSeqSpec =
  describe "Correct bracket sequence" $ do
    it "empty" $ 
      seqToStr (runParser bracketParser "")
      `shouldBe`
      Just ("", "")

    it "parse ()" $
      seqToStr (runParser bracketParser "()")
      `shouldBe`
      Just ("()", "")

    it "parse ()()" $
      seqToStr (runParser bracketParser "()()")
      `shouldBe`
      Just ("()()", "")

    it "parse inner" $
      seqToStr (runParser bracketParser "(((())))")
      `shouldBe`
      Just ("(((())))", "")

    it "parse concatenations with inner" $
      seqToStr (runParser bracketParser "(())()(()(()))()()")
      `shouldBe`
      Just ("(())()(()(()))()()", "")

    it "positive balance" $
      seqToStr (runParser bracketParser "((()")
      `shouldBe`
      Nothing

    it "negative balance" $
      seqToStr (runParser bracketParser "()))")
      `shouldBe`
      Nothing

    it "non-bracket symbols" $
      seqToStr (runParser bracketParser "()-: wtf")
      `shouldBe`
      Nothing

numberParserTree :: IO TestTree
numberParserTree = testSpec "Task 6-3 Num Parser" numberSpec

checkNumber :: Int -> Int -> Bool
checkNumber n s =
  let trash = "other is trash" ++ show s
  in runParser numberParser (show n ++ trash) == Just (n, trash)
   
numberSpec :: Spec
numberSpec =
  describe "Number parser" $ do
    it "parses ordinary number" $
      runParser numberParser "123456789"
      `shouldBe`
      Just (123456789 :: Int, "")

    it "empty input" $
      runParser numberParser ""
      `shouldBe`
      (Nothing :: Maybe (Int, String))

    it "parse max int" $
      runParser numberParser "9223372036854775807"
      `shouldBe`
      Just (9223372036854775807 :: Integer, "")

    it "parse min int" $
      runParser numberParser "-9223372036854775808"
      `shouldBe`
      Just (-9223372036854775808 :: Integer, "")

    it "parse prefix" $
      runParser numberParser "8800-76kekw"
      `shouldBe`
      Just (8800 :: Int, "-76kekw")

    it "parse positive num" $
      runParser numberParser "+101"
      `shouldBe`
      Just (101 :: Int, "")

    it "parse negative num" $
      runParser numberParser "-101"
      `shouldBe`
      Just (-101 :: Int, "")

    it "parse prefix numbers" $
      property checkNumber