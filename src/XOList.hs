module XOList where

import Data.Monoid
import Control.Monad

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty =
    Nil
  mappend xs ys =
    case (xs, ys) of
      (Nil, ys) ->
        ys
      (Cons x xs, ys) ->
        Cons x $ mappend xs ys


instance Functor List where
  fmap f s =
    case s of
      Nil ->
        Nil
      Cons x xs ->
        Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) fs vs =
    case (fs, vs) of
      (Nil, _) ->
        Nil
      (_, Nil) ->
        Nil
      (Cons hd tl, ys) ->
        (hd <$> ys) <> (tl <*> ys)

