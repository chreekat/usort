{-# LANGUAGE DeriveFunctor #-}
module Sorted where

fromLeft (S x p) = S x (L p)
fromRiht (S x p) = S x (R p)

data Provenance
        = L Provenance
        | R Provenance
        | B

instance Show Provenance where
    show B = "B"
    show (L p) = "L:" ++ show p
    show (R p) = "R:" ++ show p


data Sorted a = S a Provenance
    deriving (Functor)

instance Show a => Show (Sorted a) where
    show (S a p) = show a ++ ":" ++ show p

val (S a _) = a
