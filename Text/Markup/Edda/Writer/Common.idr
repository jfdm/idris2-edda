|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Writer.Common

import System.File
import Data.SortedMap

import Text.Markup.Edda.Model

export
strFromMaybe : (a -> String) -> Maybe a -> String
strFromMaybe _ Nothing  = ""
strFromMaybe f (Just x) = f x

export
toString : (List String -> String)
        -> (Pair String String -> String)
        -> SortedMap String String
        -> String
toString flatten toStr kvs
  = flatten $ map toStr (kvList kvs)

export
writeEddaFile : (Edda DOC -> String)
             -> String
             -> Edda DOC
             -> IO (Either FileError ())
writeEddaFile write fname doc
  = writeFile fname (write doc)

-- [ EOF ]
