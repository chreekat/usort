{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

data MergeState = MergeState
        { _stacc :: [Text]
          -- ^ accumulator for current merge
        , _stleft :: NonEmpty Text
          -- ^ left workspace
        , _stright :: NonEmpty Text
          -- ^ right workspace
        , _strest :: [NonEmpty Text]
          -- ^ lists left to process
        }
    deriving (Eq, Show, Generic)
