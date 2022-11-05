|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Writer.Common

import System.File

import Text.Markup.Edda.Model

export
strFromMaybe : (a -> String) -> Maybe a -> String
strFromMaybe f Nothing  = "EMPTY"
strFromMaybe f (Just x) = f x

export
writeEddaFile : (Edda DOC -> String)
             -> String
             -> Edda DOC
             -> IO (Either FileError ())
writeEddaFile write fname doc = writeFile fname (write doc)

-- [ EOF ]
