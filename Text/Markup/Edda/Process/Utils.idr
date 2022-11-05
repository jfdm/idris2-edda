|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Process.Utils

import Data.SortedMap

%default total

export
lookupType : SortedMap String String -> Maybe String
lookupType = lookup "type"

export
lookupSrcLang : SortedMap String String -> Maybe String
lookupSrcLang = lookup "src_lang"

export
lookupSrcOpts : SortedMap String String -> Maybe String
lookupSrcOpts = lookup "src_opts"

export
nubAttribute : String -> SortedMap String String -> SortedMap String String
nubAttribute key as = fromList $ doNub key (toList as)
  where
    doNub : String -> List (String, String) -> List (String, String)
    doNub _   Nil     = Nil
    doNub key ((k,v)::xs) =
      case key == k of
        True => doNub key xs
        False => (k,v) :: doNub key xs

-- --------------------------------------------------------------------- [ EOF ]
