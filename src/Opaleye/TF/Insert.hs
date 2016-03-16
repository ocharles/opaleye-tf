{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Insert where

-- | 'Insertion' witnesses that a table is being @INSERT@ed. This adds special
-- meaning to 'PGDefault''.
data Insertion :: k -> * where
