{-# LANGUAGE DeriveFunctor #-}
module Sorted where

fromLeft, fromRiht :: Sorted a -> Sorted a
fromLeft (S x p) = S x (L p)
fromRiht (S x p) = S x (R p)

data Provenance
        = L Provenance
        | R Provenance
        | B
    deriving (Eq)

instance Show Provenance where
    show B = "B"
    show (L p) = "L:" ++ show p
    show (R p) = "R:" ++ show p

data Sorted a = S a Provenance
    deriving (Functor, Eq)

instance Show a => Show (Sorted a) where
    show (S a p) = show a ++ ":" ++ show p

val :: Sorted a -> a
val (S a _) = a

base :: a -> Sorted a
base x = S x B
