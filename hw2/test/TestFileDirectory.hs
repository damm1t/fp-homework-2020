module TestFileDirectory
  ( fdSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import FileDirectory
import Data.Time
import qualified System.Directory.Internal as SI
import qualified Data.ByteString.Char8 as BS



file = File { path = "/root/file.txt"
            , fileData = BS.pack "file content"
            , fileInfo = FileInfo { filePath = "/root/file.txt"
                                  , ext = ".txt"
                                  , fileSize = 12
                                  , fileTime = "2020-05-08 18:42:33.933213009"
                                  , filePerm = SI.Permissions {SI.readable = True, SI.writable = True, SI.executable = False, SI.searchable = True}
                                  }

            }


file2 = File { path = "/root/file2.txt"
            , fileData = BS.pack "file2 content"
            , fileInfo = FileInfo { filePath = "/root/file2.txt"
                                  , ext = ".txt"
                                  , fileSize = 13
                                  , fileTime = "2020-05-08 18:42:33.933213009"
                                  , filePerm = SI.Permissions {SI.readable = True, SI.writable = True, SI.executable = False, SI.searchable = True}
                                  }

            }

tree = Dir { path = "/root"
           , children = [file]
           , dirInfo = DirInfo { dirPath = "/root"
                               , dirSize = 12
                               , dirCount = 1
                               , dirPerm = SI.Permissions { SI.readable = True
                                                          , SI.writable = True
                                                          , SI.executable = False
                                                          , SI.searchable = True
                                                          }
                               }
           }

tree2 = Dir { path = "/root"
           , children = [file, file2]
           , dirInfo = DirInfo { dirPath = "/root"
                               , dirSize = 25
                               , dirCount = 2
                               , dirPerm = SI.Permissions { SI.readable = True
                                                          , SI.writable = True
                                                          , SI.executable = False
                                                          , SI.searchable = True
                                                          }
                               }
           }
fdSpec :: SpecWith ()
fdSpec =
  describe "FileDirectory" $ do
    it "test isTree" $ do
      isDir tree `shouldBe` True
      isDir file `shouldBe` False
    it "test getTreeName" $ do
      getTreeName tree `shouldBe` "root"
      getTreeName file `shouldBe` "file.txt"
    it "test addToTree" $ addToTree (tree, path tree) file2 `shouldBe` tree2
    it "test removeFromTree" $ removeFromTree (tree2, path tree2) (path file2) `shouldBe` tree
    it "test hasNext" $ 
      do
        hasNext (path file2) (children tree2)  `shouldBe` Just file2
        hasNext (path tree) (children tree2)  `shouldBe` Nothing
