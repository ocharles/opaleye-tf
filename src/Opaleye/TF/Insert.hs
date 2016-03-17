{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Insert where

import Opaleye.TF.Machinery

-- | 'Insertion' witnesses that a table is being @INSERT@ed. This adds special
-- meaning to 'PGDefault''.
data Insertion :: k -> * where

type family InsertionTyfun (col :: t) :: (TyFun t * -> *)
