{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.TF.Expr where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op

newtype Expr (columnType :: k) = Expr Op.PrimExpr

type family ExprType (f :: j -> *) (col :: k) :: *

-- | Function spaces within PostgreSQL.
data a ~> b
  = Cast   -- ^ This 'function' is simply a cast between expressions.
  | Id

mapExpr :: (a ~> b) -> Expr a -> Expr b
mapExpr Cast (Expr a) = Expr a
mapExpr Id (Expr a) = Expr a
