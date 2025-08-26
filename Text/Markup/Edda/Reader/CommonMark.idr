||| Common functions for parsing.
|||
||| Copyright : COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Reader.CommonMark

import Data.List
import Data.Nat
import Text.Quantity
import Text.Parser

import Text.Markup.Edda
import Text.Markup.Edda.Process
import Text.Markup.Edda.Reader.API

%default total

CMarkSpec : MarkdownSpec
CMarkSpec
  = MSpec ["*", "_", "-", "+", "#", "~", "<", ">", "`", "[", "]", "(", ")", "!", "."]
          Nil
          Nothing
          (Just (MkPair "<!--" "-->"))

threeSpaces : Rule ()
threeSpaces
  = do a <- spaceInline -- count expects at least one
       a <- optional $ spaceInline
       a <- optional $ spaceInline
       pure ()


spacesEOL : Rule ()
spacesEOL
  = do xs <- manyTill (eol) spaceInline
       pure ()

-- Inline

text : Rule (EddaRaw INLINE)
text
  = map (Font SerifTy) Edda.text

space : Rule (EddaRaw INLINE)
space
  = map (Punc) spaceInline

punc : Rule (EddaRaw INLINE)
punc
  = map Punc reserved' <|> map Punc punc

linkExp : Rule (EddaRaw INLINE)
linkExp
  = do reserved '<'
       url <- someTill (reserved '>') any
       pure $ Link ExposedTy (concat $ forget url) Nil

mutual

  between : Rule () -> Rule () -> Rule (List $ EddaRaw INLINE)
  between ldel rdel
    = do ldel
         ls <- someTill rdel
                        (do i <- inlineNoSpace; s <- optional space; pure (i,s))
         pure (concatMap (\(k,v) => maybe [k] (\x => [k,x]) v) ls)

  markup : MarkupTy -> Rule () -> Rule (EddaRaw INLINE)
  markup ty delim
    = do txt <- between delim delim
         pure $ Mark ty txt

  bold : Rule (EddaRaw INLINE)
  bold = (assert_total $ markup BoldTy (reserved '*' *> reserved '*'))

  em : Rule $ EddaRaw INLINE
  em = (assert_total $ markup EmphTy (reserved '*'))

  code : Rule $ EddaRaw INLINE
  code
    = do ldelim <- some (reserved '`')
         c <- someTill (myCount (length ldelim) (reserved '`'))
                       any
         pure (Raw CodeTy (concat c))

  linkHyper : Rule (EddaRaw INLINE)
  linkHyper
    = do desc <- between (reserved '[') (reserved ']')
         reserved '('
         url <- someTill (reserved ')') any
         pure $ Link HyperTy (concat url) desc

  inlineNoSpace : Rule (EddaRaw INLINE)
  inlineNoSpace
    =   bold
    <|> em
    <|> linkExp
    <|> assert_total(linkHyper)
    <|> code
    <|> text
    <|> punc

  inline : Rule (EddaRaw INLINE)
  inline = inlineNoSpace
       <|> space
  --     <?> "Raw Inline"


-- Blocks
block  : Rule (EddaRaw BLOCK)

commentBlock : Rule (EddaRaw BLOCK)
commentBlock
  = do c <- comment
       pure Empty

-- @todo three spaces
hrule : Rule (EddaRaw BLOCK)
hrule
  = hrule' '*' <|> hrule' '_' <|> hrule' '-'
  where
    hrule' : Char -> Rule (EddaRaw BLOCK)
    hrule' s
      = do reserved s
           u <- manyTill (reserved s) spaceInline
           v <- manyTill (reserved s) spaceInline
           spacesEOL
           pure HRule

-- @todo restrict to depth six
-- @todo three spaces

header : Rule (EddaRaw BLOCK)
header
  = do d <- some (reserved '#')
       commit
       t <- some inline
       o <- optional $ many (reserved '#')
       s <- manyTill (eol) space
       pure $ Heading (length d) Nothing (forget t) empty

line : Rule (List (EddaRaw INLINE))
line = do ls <- someTill eol inline
          pure (forget ls ++ ([Punc '\n']))

para : Rule (EddaRaw BLOCK)
para
  = do txt <- some line
       pure $ TextBlock ParaTy Nothing Nil empty (concat $ forget $ txt)

-- @todo three spaces
fencedCode : Rule (EddaRaw BLOCK)
fencedCode
    = code (reserved '`') <|> code (reserved '~')
  where
    code : Rule () -> Rule (EddaRaw BLOCK)
    code g
      = do cs <- myCount 3 g
           ds <- many g
           lang <- Edda.text
           c <- spaceInline
           ls <- manyTill eol any
           raw <- Parser.manyTill (myCount (3 + length ds) g)
                           any
           eol
           pure (VerbBlock ListingTy Nothing Nil
                           (insert "lang" lang $ insert "options" (concat ls) empty) (concat raw))


indentedCode : Rule (EddaRaw BLOCK)
indentedCode
  = do cs <- myCount 4 spaceInline
       l <- manyTill eol any
       ls <- many (do cs <- myCount 4 spaceInline
                      raw <- manyTill eol any
                      pure (concat raw))

       pure (VerbBlock LiteralTy Nothing Nil empty (unlines $ concat l::ls))

-- @todo three spaces
-- @todo contain blocks
blockquote : Rule (EddaRaw BLOCK)
blockquote
  = do txt <- someTill eol (reserved '>' *> spaceInline *> inline)
       pure (TextBlock (Named "blockquote") Nothing Nil empty (forget txt))

figure : Rule (EddaRaw BLOCK)
figure
  = do reserved '!'
       desc <- between (reserved '[') (reserved ']')
       reserved '('
       url <- someTill (reserved ')') any
       eol
       pure (Figure (Just "") desc empty $ Link ExposedTy (concat url) Nil)

-- @todo make blocks
listG : ListTy -> Rule (DPair Nat IsSucc) ->  Rule (EddaRaw BLOCK)
listG ty mark
  = do tts <- some $ listItem mark
       pure $ ListBlock ty (forget tts)
  where

    listLine : DPair Nat IsSucc -> Rule (List (EddaRaw INLINE))
    listLine ((S n) ** ItIsSucc)
      = do n <- myCount (S n) spaceInline
           t <- line
           pure t

    listItem : Rule (DPair Nat IsSucc) -> Rule (List (EddaRaw INLINE))
    listItem mark
      = do n <- mark
           t <- line
           ts <- many (listLine n)
           pure (concat $ t::ts)

uList : Rule (EddaRaw BLOCK)
uList
  =      listG BulletTy (umarkerG '*')
    <|>  listG BulletTy (umarkerG '+')
    <|>  listG BulletTy (umarkerG '-')
  where
    umarkerG : Char -> Rule (DPair Nat IsSucc)
    umarkerG c
      = do reserved c
           c <- spaceInline
           commit
           pure (2 ** ItIsSucc)

oList : Rule (EddaRaw BLOCK)
oList
  =      listG NumberTy (umarkerG '.')
    <|>  listG NumberTy (umarkerG ')')
  where
    umarkerG : Char -> Rule (DPair Nat IsSucc)
    umarkerG p
      = do m <- count (atMost 9) digit
           reserved p
           c <- spaceInline
           commit
           pure (S (S (length m)) ** ItIsSucc)

block
  = commentBlock
  <|> header
  <|> blockquote
  <|> indentedCode
  <|> fencedCode
  <|> hrule
  <|> uList
  <|> oList
  <|> figure
  <|> para

commonMark : Rule (EddaRaw DOC)
commonMark
  = do txt <- some (block <* eol <* many eol)
       t <- option Empty block
       pure $ Doc Nil empty (forget txt ++ [t])


export
covering
parseFile : String -> IO $ Either String (Edda DOC)
parseFile
  = Edda.parseFile CMarkSpec commonMark


-- [ EOF ]
