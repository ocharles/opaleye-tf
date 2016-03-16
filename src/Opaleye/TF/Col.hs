{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.TF.Col where

import Data.Proxy
import Opaleye.TF.Expr
import Opaleye.TF.Insert
import Opaleye.TF.Interpretation
import Opaleye.TF.Machinery
import Opaleye.TF.Nullable
import Opaleye.TF.Table

-- | The workhorse type family in @opaleye-tf@. This type family "collapses"
-- noisy types down into things that are more meaningful. For example, when
-- used with 'Interpret' you just get ordinary Haskell values.
type family Col (f :: a -> b) (x :: a) :: b where
  Col Interpret x = Apply (HaskellTyfun x) x  -- TODO In GHC 8 I believe we can do Col Interpret (x :: k) = Apply (Interpretation k) x
  Col (Compose Interpret 'Nullable) x = Apply (HaskellNullableTyfun x) x

  Col Insertion x = Apply (InsertionTyfun x) x

  Col Expr x = Apply (ExprTyfun x) x
  Col (Compose Expr 'Nullable) x = Apply (NullableExprTyfun x) x

  Col ExtractSchema x = Proxy (Apply (ExtractColumnNameTyfun x) x)
