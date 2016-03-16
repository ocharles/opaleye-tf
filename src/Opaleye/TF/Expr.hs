{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Expr where

import Opaleye.TF.Machinery
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op

newtype Expr (columnType :: k) = Expr Op.PrimExpr

type family ExprTyfun (col :: t) :: (TyFun t * -> *)
type family NullableExprTyfun (col :: t) :: (TyFun t k -> *)
