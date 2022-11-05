|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Process.Shunt

import Data.SortedMap
--import Data.SnocList
import Data.List.Views

import Text.Markup.Edda.Model.Common
import Text.Markup.Edda.Model.Processed


%default total

-- @ TODO make more precise using SnocLists

data SecInfo : Type where
  MkSInfo : (depth : Nat)
         -> (label : Maybe String)
         -> (title : List (Edda INLINE))
         -> (attrs : SortedMap String String)
         -> (body  : List (Edda BLOCK))
         -> SecInfo
  NoInfo : SecInfo

popThings : Nat -> List (Edda BLOCK)
                -> List (Edda BLOCK)
                -> (List (Edda BLOCK), List (Edda BLOCK))
popThings k xs Nil = (xs, Nil)
popThings k xs ((Section d l t as b)::ys) with (compare d k)
  popThings k xs ((Section d l t as b)::ys) | LT = (xs, Section d l t as b :: ys)
  popThings k xs ((Section d l t as b)::ys) | _ = popThings k (xs ++ [Section d l t as b]) ys

popThings k xs (block::ys) = popThings k (xs ++ [block]) ys



performShunt : (stack  : List (Edda BLOCK))
            -> (output : List (Edda BLOCK))
            -> (input  : List (Edda BLOCK))
                      -> List (Edda BLOCK)
performShunt stack output input with (snocList input)
  performShunt stack output [] | Empty = stack ++ output

  performShunt stack output (xs ++ [x]) | (Snoc x xs rec) with (x)

    performShunt stack output (xs ++ [x]) | (Snoc x xs rec) | (Section depth label title attrs body) with (output)
      performShunt stack output (xs ++ [x]) | (Snoc x xs rec) | (Section depth label title attrs body) | []
        = performShunt [] [Section depth label title attrs (body ++ stack)] xs | rec

      performShunt stack output (xs ++ [x]) | (Snoc x xs rec) | (Section depth label title attrs body) | (y :: ys) with (y)
        performShunt stack output (xs ++ [x]) | (Snoc x xs rec) | (Section depth label title attrs body) | (y :: ys) | (Section k mstr zs z body1)
          = case compare depth k of
              LT => let (ls,rs) = popThings k Nil (output)
                    in performShunt [] ([Section depth label title attrs (body ++ stack ++ ls)] ++ rs) xs | rec

              EQ => performShunt [] ([Section depth label title attrs (body ++ stack)] ++ output) xs | rec
              GT => performShunt [] ([Section depth label title attrs (body ++ stack)] ++ output) xs | rec

        performShunt stack output (xs ++ [x]) | (Snoc x xs rec) | (Section depth label title attrs body) | (y :: ys) | block
          = performShunt [] ([Section depth label title attrs (body ++ stack)] ++ output) xs | rec


    performShunt stack output (xs ++ [x]) | (Snoc x xs rec) | block
      = performShunt (block :: stack) output xs | rec

export
shunt : List (Edda BLOCK) -> List (Edda BLOCK)
shunt = performShunt Nil Nil

-- --------------------------------------------------------------------- [ EOF ]
