{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.TF.Expr where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op

newtype Expr (columnType :: k) = Expr Op.PrimExpr

type family ExprType (f :: j -> *) (col :: k) :: *
