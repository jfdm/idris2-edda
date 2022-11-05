|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Model.Raw

import Data.String
import Data.SortedMap

import Text.Markup.Edda.Model.Common

%default total

public export
data EddaRaw : EddaTy -> Type where

  -- [ Inline ]
  -- [ Raw ]
  Font : (ty   : FontTy)
      -> (word : String)
              -> EddaRaw INLINE

  Punc : (c : Char)
           -> EddaRaw INLINE

  Link : (ty   : LinkTy)
      -> (url  : String )
      -> (desc : List (EddaRaw INLINE))
              -> EddaRaw INLINE

  Mark : (ty   : MarkupTy)
      -> (text : List (EddaRaw INLINE))
              -> EddaRaw INLINE

  Raw  : (ty   : RawTy)
      -> (verb : String)
              -> EddaRaw INLINE

  -- [ Blocks ]

  HRule : EddaRaw BLOCK
  Empty : EddaRaw BLOCK

  Figure : (label   : Maybe String)
        -> (caption : List (EddaRaw INLINE))
        -> (attrs   : SortedMap String String)
        -> (link    : EddaRaw INLINE)

                   -> EddaRaw BLOCK

  DList : (kvpairs : List (List (EddaRaw INLINE), List (EddaRaw INLINE)))
                  -> EddaRaw BLOCK

  Heading : (depth : Nat)
         -> (label : Maybe String)
         -> (title : List (EddaRaw INLINE))
         -> (attrs : SortedMap String String)
                  -> EddaRaw BLOCK

  TextBlock : (ty    : TextBlockTy)
           -> (label : Maybe String)
           -> (title : List (EddaRaw INLINE))
           -> (attrs : SortedMap String String)
           -> (text  : List (EddaRaw INLINE))
                    -> EddaRaw BLOCK

  VerbBlock : (ty      : VerbBlockTy)
           -> (label   : Maybe String)
           -> (caption : List (EddaRaw INLINE))
           -> (attrs   : SortedMap String String)
           -> (content : String)
                      -> EddaRaw BLOCK

  ListBlock : (ty    : ListTy)
           -> (items : List (List (EddaRaw INLINE)))
                    -> EddaRaw BLOCK

  Snippet : (content : List $ EddaRaw a)
         -> (prf     : ValidSnippet a)
                    -> EddaRaw SNIPPET

  Doc : (title : List (EddaRaw INLINE)) -- @TODO resolve author and dates
     -> (attrs : SortedMap String String)
     -> (body  : List (EddaRaw BLOCK))
              -> EddaRaw DOC

-- INLINE | BLOCK | SNIPPET | DOC
public export
data View : (0 type : EddaTy) -> (raw : EddaRaw type) -> Type where
  IsInLine  : (raw : EddaRaw INLINE)  -> View INLINE  raw
  IsBlock   : (raw : EddaRaw BLOCK)   -> View BLOCK   raw
  IsSnippet : (raw : EddaRaw SNIPPET) -> View SNIPPET raw
  IsDoc     : (raw : EddaRaw DOC)     -> View DOC     raw

export
view : (raw : EddaRaw type) -> View type raw
view (Font ty word) = IsInLine (Font ty word)
view (Punc c) = IsInLine (Punc c)
view (Link ty url desc) = IsInLine (Link ty url desc)
view (Mark ty text) = IsInLine (Mark ty text)
view (Raw ty verb) = IsInLine (Raw ty verb)
view HRule = IsBlock HRule
view Empty = IsBlock Empty
view (Figure label caption attrs link) = IsBlock (Figure label caption attrs link)
view (DList kvpairs) = IsBlock (DList kvpairs)
view (Heading depth label title attrs) = IsBlock (Heading depth label title attrs)
view (TextBlock ty label title attrs text) = IsBlock (TextBlock ty label title attrs text)
view (VerbBlock ty label caption attrs content) = IsBlock (VerbBlock ty label caption attrs content)
view (ListBlock ty items) = IsBlock (ListBlock ty items)
view (Snippet content prf) = IsSnippet (Snippet content prf)
view (Doc title attrs body) = IsDoc (Doc title attrs body)

export
Show QuoteTy where
  show SQuote = "SQuote"
  show DQuote = "DQuote"

export
Show CiteSty where
  show ParenSty = "ParenCite"
  show TextSty  = "TextCite"

export
Show ParenTy where
  show Parents = "Parens"
  show Brackets = "Brackets"
  show Braces = "Braces"

export
Show FontTy where
  show SerifTy = "Serif"
  show SansTy  = "Sans"
  show ScapTy  = "SmallCaps"
  show MonoTy  = "Monospaced"

export
Show LinkTy where
  show HyperTy   = "HyperLink"
  show ExposedTy = "Exposed"
  show FnoteTy   = "Footnote"
  show RefTy     = "Internal"
  show CiteTy    = "Citation"

export
Show MarkupTy where
  show BoldTy   = "Strong"
  show EmphTy   = "Emph"
  show StrikeTy = "Strike"
  show UlineTy  = "Uline"

export
Show RawTy where
  show VerbTy = "Verb"
  show CodeTy = "Code"
  show MathTy = "Math"

export
Show TextBlockTy where
  show ParaTy        = "PARAGRAPH"
  show TheoremTy     = "THEOREM"
  show CorollaryTy   = "COROLLARY"
  show LemmaTy       = "LEMMA"
  show PropositionTy = "PROPOSITION"
  show ProofTy       = "PROOF"
  show DefinitionTy  = "DEFINITION"
  show ExampleTy     = "EXAMPLE"
  show ExerciseTy    = "EXERCISE"
  show NoteTy        = "NOTE"
  show ProblemTy     = "PROBLEM"
  show QuestionTy    = "QUESTION"
  show RemarkTy      = "REMARK"
  show SolutionTy    = "SOLUTION"
  show QuotationTy   = "QUOTATION"

export
Show VerbBlockTy where
  show CommentTy  = "COMMENT"
  show ListingTy  = "LISTING"
  show LiteralTy  = "LITERTAL"
  show EquationTy = "EQUATION"

export
Show ListTy where
  show BulletTy = "Bullet"
  show NumberTy = "Number"

private
partial
showInline : EddaRaw INLINE -> String
showInline (Punc c)    = unwords ["{Punc", show c,  "}"]
showInline (Font ty t) = unwords ["{Font", show ty, show t, "}"]
showInline (Raw ty t)  = unwords ["{Raw",  show ty, show t, "}"]

showInline (Mark ty ts) = unwords ["{Mark"
                                      , show ty
                                      , concatMap showInline ts
                                      , "}"]

showInline (Link ty u ts) = unwords ["{Link"
                                        , show ty
                                        , "<" ++ u ++ ">"
                                        , show $ concatMap showInline ts
                                        , "}"]



private
partial
showBlock : EddaRaw BLOCK -> String
showBlock (HRule) = "[HRule]"
showBlock (Empty) = "[Empty]"
showBlock (TextBlock ty lab cap as txt) = unwords
    ["[TextBlock"
    , show ty
    , show lab
    , concatMap showInline cap
    , show as
    , concatMap showInline txt
    , "]"]
showBlock (VerbBlock ty lab cap as txt) = unwords
    ["[VerbBlock"
    , show ty
    , show lab
    , concatMap showInline cap
    , show as
    , show txt
    , "]"]
showBlock (ListBlock ty iis) = unwords
    [ "[BList"
    , show ty
    , concatMap (\is => unwords ["[Item", concatMap showInline is, "]"]) iis
    , "]"]
showBlock (Heading d l t a) = unwords
    [ "[Heading"
    , show d
    , show l
    , concatMap showInline t
    , show a
    , "]"]
showBlock (Figure l c as img) = unwords
    [ "[Figure"
    , show l
    , concatMap showInline c
    , show as
    , showInline img
    , "]"]
showBlock (DList ds) = unwords
    [ "[DList"
    , concatMap (\(k,vs) => concatMap showInline k ++ " " ++ concatMap showInline vs) ds
    , "]"]

export
Show (EddaRaw ty) where
  show doc with (view doc)
    show doc | (IsInLine doc)
      = assert_total $ showInline doc

    show doc | (IsBlock doc)
      = assert_total $ showBlock doc

    show (Snippet content IsInLine) | (IsSnippet (Snippet content IsInLine))
      = assert_total $ unwords $ ["[Snippet "] ++ [show content] ++ [" IsInLine]"]

    show (Snippet content IsBlock) | (IsSnippet (Snippet content IsBlock))
      = assert_total $ unwords $ ["[Snippet "] ++ [show content] ++ [" IsBlock]"]

    show (Doc title attrs body) | (IsDoc (Doc title attrs body))
      = assert_total $ unwords $ ["[Doc "] ++ [show title, show attrs, show body] ++ ["]"]

-- [ EOF ]
