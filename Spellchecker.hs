module Spellchecker where

import Control.Monad (forever)
import System.IO

import qualified Data.Set as Set

type Dictionary = Set.Set String

main :: IO ()
main =
  do dict <- readDictionary "words"
     spellCheckLoop dict

readDictionary :: FilePath -> IO Dictionary
readDictionary file =
  do text <- readFile file
     return $ Set.fromList (lines text)

spellCheckLoop :: Dictionary -> IO ()
spellCheckLoop dict =
 do hSetBuffering stdout NoBuffering
    forever (spellCheck dict)

spellCheck :: Dictionary -> IO ()
spellCheck dict =
  do putStr "> "
     word <- getLine
     let result =
           if word `Set.member` dict
             then "found!"
             else "not found :("
     putStrLn ("the word was " ++ result)

