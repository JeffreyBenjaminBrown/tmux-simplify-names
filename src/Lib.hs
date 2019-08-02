{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import System.Process
import Text.Regex


trailing_number :: Regex
trailing_number = mkRegex "-[0-9]*$"

go :: IO ()
go = do
  tmux_names :: [String] <-
    map (takeWhile $ not . (==) ':') .
    lines <$>
    readProcess "tmux" ["ls"] ""
  let tmux_new_names :: [String] =
        map ( head .
              splitRegex trailing_number )
        tmux_names
      pairs :: [(String,String)] =
        zip tmux_names tmux_new_names
      do_pair :: (String,String) -> CreateProcess
      do_pair (old,new) = shell $
        "tmux rename-session -t " ++ old ++ " " ++ new
  mapM_ (createProcess . do_pair) pairs
