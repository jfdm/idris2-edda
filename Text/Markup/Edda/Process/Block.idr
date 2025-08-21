|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Process.Block

import Data.SortedMap

import Text.Markup.Edda.Model.Common
import Text.Markup.Edda.Model.Raw
import Text.Markup.Edda.Model.Processed

import Text.Markup.Edda.Process.Inline
import Text.Markup.Edda.Process.Squash
import Text.Markup.Edda.Process.Shunt
import Text.Markup.Edda.Process.Utils

%default total

-- ----------------------------------------------------------- [ Refine Blocks ]

mutual

  refineBlock : EddaRaw BLOCK -> Edda BLOCK
  refineBlock HRule = HRule

  refineBlock Empty = Empty

  refineBlock (Heading d l t as) = Section d l (process t) as Nil

  refineBlock (Figure l c as img) =
      Figure l (process c) as url
    where
       url : Edda INLINE
       url = case process [img] of
               Nil => Text "Shouldn't happen"
               (res::_) => res


  refineBlock (DList kvs) = DList $ map (\(k, vs) => (process k, process vs)) kvs

  refineBlock (TextBlock ParaTy        l c as t) = Para (process t)
  refineBlock (TextBlock (Named n)     l c as t) = Named n l (process c) (process t)

  refineBlock (VerbBlock CommentTy l c as s) = Comment s
  refineBlock (VerbBlock ListingTy l c as s) = Listing l (process c)
                                                         (lookupSrcLang as)
                                                         (lookupSrcOpts as)
                                                         ((nubAttribute "src_lang" $ nubAttribute "src_opts" as))
                                                         s
  refineBlock (VerbBlock LiteralTy  l c as s) = Literal l (process c) s
  refineBlock (VerbBlock EquationTy l c as s) = Equation l s

  refineBlock (ListBlock NumberTy bs) = OList $ map process bs
  refineBlock (ListBlock BulletTy bs) = BList $ map process bs

  refineBlocks : List (EddaRaw BLOCK) -> List (Edda BLOCK)
  refineBlocks = map refineBlock


export
process : List (EddaRaw BLOCK) -> List (Edda BLOCK)
process
  = shunt . squash2 . refineBlocks

-- [ EOF ]
