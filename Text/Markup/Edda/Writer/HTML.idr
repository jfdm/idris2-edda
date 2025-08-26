|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Writer.HTML

import Data.String
import Data.Maybe
import Data.Nat
import System.File

import Data.SortedMap

import Text.Markup.Edda.Model

import Text.Markup.Edda.Writer.Common

%default total


-- ------------------------------------------------------------ [ Misc Writing ]

tagOpen' : String -> String
tagOpen' x
  = "<\{x}>"


tagOpen: String -> SortedMap String String -> String
tagOpen str kvs
  = if isNil (SortedMap.toList kvs)
    then tagOpen' (trim str)
    else "<\{trim str} \{toString concat mkPair kvs}>"

  where mkPair : Pair String String -> String
        mkPair (k,v) = "\{k}=\"\{v}\""


tagClosed : String -> String
tagClosed x
  = "</\{x}>"

tagEmpty : String -> String
tagEmpty x = "<\{x}/>"

tag : (a -> String)
   -> String
   -> SortedMap String String
   -> a
   -> String
tag f n kvs body
  = concat [ tagOpen n kvs
            , f body
            , tagClosed n
            ]

tagSimple : (a -> String)
   -> String
   -> a
   -> String
tagSimple f n body
  = concat [ tagOpen' n
            , f body
            , tagClosed n
            ]

tag' : String
    -> String
    -> String
tag' n body
  = unlines [ tagOpen n empty
            , body
            , tagClosed n
            ]

wrap : (l,r : String) -> String -> String
wrap l r this = "\{l}\{this}\{r}"

-- ----------------------------------------------------------- [ Write Inlines ]


inline : Edda INLINE -> String

inlines : List (Edda INLINE) -> String
inlines
  = assert_total $ concatMap inline

link : String -> List (Edda INLINE) -> String
link uri Nil
  = tag' "p" uri
link uri desc
  = tag inlines
        "a"
        (insert "href" uri $ empty)
        desc


inline (Text t) = t
inline (Sans t) = t
inline (Scap t) = t

-- @TODO make better
inline (Mono t) = tag' "code" t
inline (Verb v) = tag' "code" v
inline (Code v) = tag' "code" v
inline (Math v) = tag' "code" v

inline (Emph t)     = tagSimple inlines "em"  t
inline (Bold t)     = tagSimple inlines "strong"  t
inline (Strike t)   = tagSimple inlines "s"  t
inline (Uline t)    = tagSimple inlines "u"  t
inline (Quote ty t) =
  case ty of
    SQuote => wrap "&lsquo;" "&rsquo;" (inlines t)
    DQuote => tagSimple inlines "q" t

inline (Parens ty t) =
  case ty of
    Parents  => wrap "(" ")"  (inlines t)
    Brackets => wrap "[" "]"  (inlines t)
    Braces   => wrap "{" "}"  (inlines t)
inline (Ref url)        = link url Nil
inline (Hyper uri desc) = link uri desc

-- @TODO Make better
inline (FNote l d)
  = tag' "sub" l
  --macro "footnote" (inlines d)

inline (Cite ty uri)
  = tag' "em" uri
--  case ty of
--    ParenSty => (macro "cite"  uri)
--    TextSty  => (macro "citet" uri)
inline (MiscPunc c) =
  case c of
    '%' => "%"
    '_' => "_"
    '^' => "^"
    '~' => "&tilde;"
    c   => cast c
inline Space      = " "
inline Newline    = "&nbsp;"
inline Tab        = "    "
inline LBrace     = "{"
inline RBrace     = "}"
inline LParen     = "("
inline RParen     = ")"
inline LBrack     = "["
inline RBrack     = "]"
inline LAngle     = "&lt;"
inline RAngle     = "&gt;"
inline Dollar     = "$"
inline Colon      = ":"
inline Semi       = ";"
inline EnDash     = "&ndash"
inline EmDash     = "&mdash;"
inline FSlash     = "/"
inline BSlash     = "\\"
inline Apostrophe = "'"
inline SMark      = "\""
inline Comma      = ","
inline Plus       = "+"
inline Ellipsis   = "&hellip;"
inline Hyphen     = "-"
inline Bang       = "!"
inline Period     = "."
inline QMark      = "?"
inline Hash       = "\\#"
inline Equals     = "="
inline Pipe       = "|"


secLvl : Nat -> Maybe Nat
secLvl n
  = if lte n 5 then Just n else Nothing

list : List (List (Edda INLINE)) -> String
list xxs = unlines $ map (tagSimple inlines "li") xxs

dlist : List (Pair (List (Edda INLINE)) (List (Edda INLINE))) -> String
dlist kvs
  =  tag' "dl" $ unlines $ map mkPair kvs
  where
    mkPair : (List (Edda INLINE), List (Edda INLINE)) -> String
    mkPair (k,v)
      = unlines [ tagSimple inlines "dt" k
                , tagSimple inlines "dd" v
                ]
--  env "description" (unlines $ map descItem kvs)
--  where
--    descItem : (List (Edda INLINE), List (Edda INLINE)) -> String
--    descItem (k,v) = unwords ["\\item[" ++ inlines k ++ "]", inlines v]

block : Edda BLOCK -> String

export
blocks : (List (Edda BLOCK)) -> String
blocks = assert_total $ concatMap block

block (HRule) = tagEmpty "hrule"
block (Empty) = "\n"
block (Section lvl label title as body)
  = case secLvl lvl of
      Nothing => unlines ["<!-- Unrecognised depth \{show $ S lvl} -->"
                         , tag' "p" (tag' "strong" $ inlines title)
                         , blocks body]
      Just n => unlines [ tag inlines "h\{show (S n)}" kvs title
                        , blocks body
                        ]
  where
    kvs : SortedMap String String
    kvs = maybe as (\l => insert "label" l as) label

block (Figure l c as fig)
  = figure l c fig
  where
    figure : Maybe String
          -> List (Edda INLINE)
          -> Edda INLINE
          -> String
    figure ml cap body
      = tagSimple (\x => unlines [inline x, tagSimple inlines "figcaption" cap])
                  "figure"
                  body

block (DList kvs)
   = dlist kvs
block (OList bbs)
  = tagSimple list "ol" bbs

block (BList bbs)
  = tagSimple list "ul" bbs

block (Para txt)
  = tagSimple inlines "p" txt


-- @todo -- Add support for caption and label
block (Listing l c lang langopts as src)
  = tag' "pre" (tag' "code" src)
block (Comment ss)
  = "<!--\n \{ss} \n-->"
block (Equation l eq)
  = tag' "pre" eq

block (Literal l c src)
  = tag' "pre" (tag' "code" src)

block (Named n l c txt)
  = case (toLower n) of
      "quote"     => tagSimple inlines "blockquote" txt
      "quotation" => tagSimple inlines "blockquote" txt
      n => unlines [tag' "p" (tag' "strong" n), tagSimple inlines "p" txt]



-- -------------------------------------------------------- [ Write List (String, String) ]

properties : SortedMap String String -> String
properties
  = toString unlines mkPair
  where mkPair : Pair String String -> String
        mkPair (k,v) = "\{k}=\"\{v}\""

-- --------------------------------------------------------------- [ Write Org ]

namespace Doc
  --@ TODO Add customisable preamble, and standalone
  ||| Convert document to HTML instance.
  export
  toHTML : Edda DOC -> String
  toHTML (Doc title ps body) = unlines
      [ "<!DOCTYPE html>"
      , "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"\" xml:lang=\"\">"
      , "<head>"
      , "<meta charset=\"utf-8\" />"
      , "<meta name=\"generator\" content=\"edda\" />"
      , tagSimple inlines "title" title
      , "</head>"
      , "<!-- \{properties ps} -->"
      , "<body>"

      , concatMap block body
      , "</body>"
      , "</html"
      ]

namespace File

  ||| Write HTML representation to file.
  export
  toHTML : String
           -> Edda DOC
           -> IO (Either FileError ())
  toHTML fn doc = writeEddaFile toHTML fn doc


namespace Snippet
  ||| Convert edda document to latex.
  export
  toHTML : Edda SNIPPET -> String
  toHTML (Snippet snippet prf) with (prf)
    toHTML (Snippet snippet prf) | IsInLine = inlines snippet
    toHTML (Snippet snippet prf) | IsBlock = blocks snippet

-- --------------------------------------------------------------------- [ EOF ]
