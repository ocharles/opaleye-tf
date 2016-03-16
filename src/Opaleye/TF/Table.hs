{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Table where

import Opaleye.TF.Insert
import Opaleye.TF.Expr
import Opaleye.TF.Machinery
import Opaleye.TF.Interpretation
import GHC.TypeLits (Symbol)

type family TableName (t :: k) :: Symbol

data Column a = Column Symbol a

data ExtractSchema (col :: k)

--------------------------------------------------------------------------------
type family ExtractColumnNameTyfun (col :: t) :: (TyFun t Symbol -> *)

data ExtractColumnName (f :: TyFun (Column t) Symbol)
type instance ExtractColumnNameTyfun (t :: Column k) = ExtractColumnName
type instance Apply ExtractColumnName ('Column name col) = name

--------------------------------------------------------------------------------
-- Pass-through instance to determine the type of 'Expr t' from 'Column n t'

data ExprType (f :: TyFun (Column t) k)
type instance ExprTyfun (t :: Column k) = ExprType
type instance Apply ExprType ('Column name col) = Apply (ExprTyfun col) col

--------------------------------------------------------------------------------
-- Pass-through instance to determine the Haskell type

data HaskellType (f :: TyFun (Column t) *)
type instance HaskellTyfun (t :: Column k) = HaskellType
type instance Apply HaskellType ('Column name col) = Apply (HaskellTyfun col) col

--------------------------------------------------------------------------------
-- Pass-through instance for insertion

data InsertionType (f :: TyFun (Column t) *)
type instance InsertionTyfun (t :: Column k) = InsertionType
type instance Apply InsertionType ('Column name col) = Apply (InsertionTyfun col) col
