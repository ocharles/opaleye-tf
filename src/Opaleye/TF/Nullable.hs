{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Nullable where

import Opaleye.TF.Col
import Opaleye.TF.Expr
import Opaleye.TF.Interpretation
import Opaleye.TF.Insert
import Opaleye.TF.Default
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op

-- | Indicate whether or not a column can take null values.
data PGNull t
  = NotNullable t
  | Nullable t

-- | Given a way to interpret the underlying column type to a Haskell type,
-- we can interpret a 'PGDefault' type into a Haskell type.
type instance Col Interpret ('Nullable x) = Maybe (Col Interpret x)

data Null a = Null | NotNull a

toNullable :: Expr a -> Expr ('Nullable a)
toNullable (Expr a) = Expr a

null :: Expr ('Nullable a)
null = Expr (Op.ConstExpr Op.NullLit)

data NullableExpr (col :: k)
type instance Col Expr ('NotNullable col) = Col Expr col
type instance Col Expr ('Nullable col) = Col NullableExpr col
type instance Col Interpret ('NotNullable col) = Col Interpret col
type instance Col Interpret ('Nullable col) = Maybe (Col Interpret col)
type instance Col Insertion (col :: PGNull k) = Col Expr col
type instance Col InsertionWithDefault (col :: PGNull k) = Default (Col Expr col)
