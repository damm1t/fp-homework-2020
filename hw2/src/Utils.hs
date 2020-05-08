module Utils where

import qualified Data.ByteString.Char8 as BS

addStrToBS :: String -> BS.ByteString -> BS.ByteString
addStrToBS addText bs = BS.concat [bs, BS.pack ['\n'], BS.pack addText]


duplicate :: String -> Int -> String
duplicate string n = Prelude.concat $ replicate n string