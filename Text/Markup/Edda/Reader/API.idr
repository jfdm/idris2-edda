||| Common functions for parsing.
|||
||| Copyright : COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Reader.API

import public System.File

import public Text.Lexer
import public Text.Parser

import public Data.String
import public Data.Maybe
import public Data.Nat

import public Toolkit.Data.Location
import public Toolkit.Text.Lexer.Run
import public Toolkit.Text.Parser.Support
import public Toolkit.Text.Parser.Location
import public Toolkit.Text.Parser.Run

import public Text.Markup.Edda.Model

import public Text.Markup.Edda.Reader.Borrowed
import public Text.Markup.Edda.Reader.Token
import public Text.Markup.Edda.Reader.Lexer

import Text.Markup.Edda.Process

%default total

umaybe : Eq a => a -> a -> Maybe Unit
umaybe x y = ifThenElse (x==y) (Just MkUnit) Nothing

umaybe' : Eq a => a -> a -> Maybe a
umaybe' x y = ifThenElse (x==y) (Just x) Nothing

namespace Edda
  public export
  Rule : Type -> Type
  Rule = Rule Unit Token

  public export
  RuleEmpty : Type -> Type
  RuleEmpty = RuleEmpty Unit Token


  export
  eoi : RuleEmpty Unit
  eoi = eoi isEOI
    where
      isEOI : Token -> Bool
      isEOI EndInput = True
      isEOI _ = False


  export
  text : Rule String
  text = terminal "Expected Text"
         (\x => case x of { Text s => Just s; _ => Nothing})

  export
  comment : Rule String
  comment = terminal "Expected Text"
         (\x => case x of { LineComment s => Just s; _ => Nothing})

  export
  commentBlock : Rule String
  commentBlock = terminal "Expected Text"
         (\x => case x of { BlockComment s => Just s; _ => Nothing})


  export
  spaceAll : Rule Char
  spaceAll = terminal "Expected space"
             (\x => case x of
                      WS s => case unpack s of
                                [x] => Just x
                                _   => Nothing
                      _ => Nothing)

  export
  spaceInline : Rule Char
  spaceInline = terminal "Expected space"
             (\x => case x of
                      WS "\n"   => Nothing
                      WS x => getCharLit x
                      _      => Nothing)


  export
  eol : Rule ()
  eol = terminal "Expected EOL"
             (\x => case x of
                      WS "\n"   => Just MkUnit
                      _      => Nothing)

  export
  punc : Rule Char
  punc = terminal "Expected Punc"
         (\x => case x of { Punk s => getCharLit s; _ => Nothing})

  export
  keyword : (str : String)
                -> Rule Unit
  keyword str
    = terminal ("Expected Keyword '" ++ str ++ "'")
               (\x => case x of
                             Keyword s => umaybe s str
                             _ => Nothing)

  export
  reserved : (c : Char)
                -> Rule Unit
  reserved c
    = terminal ("Expected reserved  '" ++ (cast c) ++ "'")
               (\x => case x of
                             Reserved s => maybe Nothing (\s => umaybe s c) (getCharLit s)
                             _ => Nothing)

  export
  reserved' : Rule Char
  reserved'
    = terminal ("Expected reserved as punc")
               (\x => case x of
                             Reserved s => (getCharLit s)
                             _ => Nothing)

  export
  any : Rule String
  any
    = terminal ("Expected Anything")
               (\x => case x of
                             (Text str) => Just str
                             (Punk str) => Just str
                             (Keyword str) => Just str
                             (Reserved str) => Just str
                             (LineComment str) => Just str
                             (BlockComment str) => Just str
                             (WS str) => Just str
                             _  => Nothing)


  export
  digit : Rule Char
  digit = terminal "Digit"
         (\x => case x of
                  Text s => case unpack s of
                              [c] => if isDigit c then Just c else Nothing
                              _ => Nothing
                  _ => Nothing)

  export
  myCount : (n : Nat) -> Rule ty -> Grammar () Token (isSucc n) (List ty)
  myCount Z g
    = pure []
  myCount (S n) g
    = do x <- g
         seq (myCount n g)
             (\xs => pure (x::xs))

  export
  parseString : (spec : MarkdownSpec)
             -> (rule : Rule ty)
             -> (str  : String)
                     -> Either (ParseError Token) ty
  parseString spec
    = parseString (Edda.Lexer spec)

  export
  covering
  parseFile : (spec : MarkdownSpec)
           -> (rule : Rule (EddaRaw DOC))
           -> (str  : String)
                     -> IO $ Either String (Edda DOC)
  parseFile spec r fname
    = do res <- parseFile (Edda.Lexer spec) r fname
         either (pure . Left . show)
                (pure . Right . processDoc)
                res
