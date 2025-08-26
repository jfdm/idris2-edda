||| A Token from a String.
|||
||| Copyright : COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Reader.Token

%default total


namespace Edda
  public export
  data Token = Text String
             | Punk String
             | Keyword String
             | Reserved String
             | LineComment String
             | BlockComment String
             | WS String
             | NotRecognised String
             | EndInput

export
Show Token where
  show this = "TODO"

export
Eq Token where
  (==) a b = ?rhs_eq




-- [ EOF ]
