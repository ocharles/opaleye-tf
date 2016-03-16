{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Default where

import Opaleye.TF.Insert
import Opaleye.TF.Expr
import Opaleye.TF.Interpretation
import Opaleye.TF.Machinery

-- | Indicate whether or not a column has a default value.
data PGDefault t
  = HasDefault t
  | NoDefault t

-- | Given a way to interpret the underlying column type to a Haskell type,
-- we can interpret a 'PGDefault' type into a Haskell type.
data InterpretDefault (f :: TyFun t * -> *) (g :: TyFun (PGDefault t) *)

type instance Apply (InterpretDefault f) ('HasDefault t) = Apply f t
type instance Apply (InterpretDefault f) ('NoDefault t) = Apply f t


-- | 'Default' is like 'Maybe', but only has meaning in @INSERT@ statements.
data Default a
  = InsertDefault -- ^ Use the @DEFAULT@ value for this column.
  | ProvideValue a -- ^ Override the default value by providing an explicit value.

--------------------------------------------------------------------------------
-- Pass-through instance to determine the type of 'Expr t' from 'Column n t'

data ExprType (f :: TyFun (PGDefault t) k)
type instance ExprTyfun (t :: PGDefault k) = ExprType
type instance Apply ExprType ('HasDefault col) = Apply (ExprTyfun col) col
type instance Apply ExprType ('NoDefault col) = Apply (ExprTyfun col) col

--------------------------------------------------------------------------------
-- Pass-through instance to determine the Haskell type

data HaskellType (f :: TyFun (PGDefault t) *)
type instance HaskellTyfun (t :: PGDefault k) = HaskellType
type instance Apply HaskellType ('HasDefault col) = Apply (HaskellTyfun col) col
type instance Apply HaskellType ('NoDefault col) = Apply (HaskellTyfun col) col

--------------------------------------------------------------------------------
-- Special usage of the 'Default' type

data InsertionType (f :: TyFun (PGDefault t) *)
type instance InsertionTyfun (t :: PGDefault k) = InsertionType
type instance Apply InsertionType ('HasDefault col) = Default (Apply (InsertionTyfun col) col)
type instance Apply InsertionType ('NoDefault col) = Apply (InsertionTyfun col) col
