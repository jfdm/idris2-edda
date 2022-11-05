|||
||| Copyright : see COPYRIGHT
||| License   : see LICENSE
|||
module Text.Markup.Edda.Process.Squash

import Data.SortedMap

import Text.Markup.Edda.Model.Common
import Text.Markup.Edda.Model.Raw
import Text.Markup.Edda.Model.Processed

%default total

-- Rewrite with views

-- ----------------------------------------------------------- [ Double Squash ]
scanSquash2 : (a -> a -> Maybe a) -> List a -> List a
scanSquash2 f Nil
  = Nil

scanSquash2 f (x :: xs) with (xs)
  _ | []
    = x :: xs
  _ | (y :: ys)
    = case f x y of
        Just yes => yes :: scanSquash2 f ys
        Nothing  => x   :: scanSquash2 f xs


partial
doSquash2 : (a -> a -> Maybe a) -> Nat -> List a -> List a
doSquash2 f o xs
  = let xs' = scanSquash2 f xs
    in if length xs == o
         then xs
         else doSquash2 f (length xs) xs



export
squash2By : (a -> a -> Maybe a) -> List a -> List a
squash2By squaFunc xs
  = assert_total
  $ doSquash2 squaFunc (length xs) xs


squashEddaPair : Edda ty -> Edda ty -> Maybe (Edda ty)
squashEddaPair HRule     HRule     = Just $ HRule
squashEddaPair (Para xs) (Para ys) = Just $ Para (xs ++ ys)
squashEddaPair Empty     Empty     = Just $ Empty
squashEddaPair Space     Space     = Just Space
squashEddaPair Hyphen    Hyphen    = Just EnDash
squashEddaPair _         _         = Nothing

export
squash2 : List (Edda ty) -> List (Edda ty)
squash2 = squash2By (squashEddaPair)


-- [ Triple Punc Squashing ]

export
squash3By : (a -> a -> a -> Maybe a) -> List a -> List a
squash3By _        Nil     = Nil
squash3By squaFunc (x::xs) with (xs)
  _ | (y::z::zs) = case squaFunc x y z of
                     Just yes => yes :: squash3By squaFunc zs
                     Nothing  => x :: squash3By squaFunc xs
  _ | (y::ys)    = x :: xs
  _ | Nil        = x :: xs

squashEddaTriples : Edda ty -> Edda ty -> Edda ty -> Maybe (Edda ty)
squashEddaTriples Period Period Period = Just Ellipsis
squashEddaTriples Hyphen Hyphen Hyphen = Just EmDash
squashEddaTriples _      _      _      = Nothing

export
squash3 : List (Edda ty) -> List (Edda ty)
squash3
  = assert_total
  $ squash3By (squashEddaTriples)

-- --------------------------------------------------------------------- [ EOF ]
