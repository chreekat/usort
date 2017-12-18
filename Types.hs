{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Control.Lens

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
    deriving (Eq, Show)

makeLenses ''MergeState
