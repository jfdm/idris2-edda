module Text.Markup.Edda.Reader.Lexer

import Data.List.Elem

import public Text.Lexer
import public Toolkit.Text.Lexer.Run


import public Text.Markup.Edda.Reader.Token

%default total

public export
record MarkdownSpec where
  constructor MSpec
  reserved : List String
  keywords : List String
  linecomment : Maybe String
  blockcomment : Maybe (String, String)

isReserved : Lexer
          -> List String
          -> (String -> Token)
          -> (String -> Token)
          -> TokenMap Edda.Token
isReserved l Nil g b
  = Nil
isReserved l xs g b
  = let foo = (\x => if elem x xs then g x else b x)
    in [(l, (\x => foo x ))]

hasLineComment : Maybe String -> TokenMap Edda.Token
hasLineComment
  = maybe Nil
          (\m => [(lineComment (exact m), LineComment)])

hasBlockComment : Maybe (String, String) -> TokenMap Edda.Token
hasBlockComment Nothing = Nil
hasBlockComment (Just (s,e))
  = [(blockComment (exact s) (exact e), BlockComment)]

tokenMap : MarkdownSpec -> TokenMap Edda.Token
tokenMap mspec
  = with List
    [ (space, WS) ]
    ++ hasLineComment  mspec.linecomment
    ++ hasBlockComment mspec.blockcomment
    ++ isReserved alphaNums mspec.keywords Keyword Text
    ++ isReserved symbol mspec.reserved Reserved Punk
    ++
    [
      (symbol, Punk)
    , (alphaNums, Text)
    , (any, NotRecognised)
    ]

keep : WithBounds Token -> Bool
keep (MkBounded t _ _) = True


namespace Edda

  export
  Lexer : MarkdownSpec -> Lexer Token
  Lexer spec = MkLexer (Reader.Lexer.tokenMap spec) keep EndInput
