module Main

import System

import Text.Markup.Edda

import Text.Markup.Edda.Reader.CommonMark
import Text.Markup.Edda.Writer.HTML

-- @todo the rug

covering
main : IO ()
main
  = do args <- getArgs
       case args of
         (x::y::xs) => do Right doc <- parseFile y
                            | Left err => putStrLn err
                          putStrLn (toHTML doc)
         _ => do printLn "Need exactly one input file"
                 exitSuccess

       exitSuccess
