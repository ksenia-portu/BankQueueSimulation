module Log where

import qualified Data.ByteString.Char8 as S8
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

logInfo :: String -> IO ()
logInfo = S8.putStrLn . encodeUtf8 . pack