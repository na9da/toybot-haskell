{-# LANGUAGE OverloadedStrings #-}

import Protolude
import Toybot
import Data.Text (lines)
import Data.List ((!!))

main :: IO ()
main = do
  args <- getArgs
  commands <- (catMaybes . fmap parseCommand) <$> readLines (args !! 0)
  bot <- run defaultBot (Table 6 6) commands
  pure ()
  where
    readLines :: FilePath -> IO [Text]
    readLines = fmap lines . readFile
  
