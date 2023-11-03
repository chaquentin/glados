-- |
-- Module      : FileProc
-- Description : Cleanup a file.
module FileProc
  ( processFile,
  )
where

import Data.Char (isSpace)

-- | Cleanup a file.
processFile :: FilePath -> IO String
processFile fileContent =
  unlines . map trim . filter (not . isEmpty) . lines <$> readFile fileContent

isEmpty :: String -> Bool
isEmpty = all isSpace

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace