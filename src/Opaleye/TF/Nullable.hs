{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Nullable where

import qualified Opaleye.Column as Op
import qualified Opaleye.Internal.Column as Op
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op
import Opaleye.TF.Col
import Opaleye.TF.Default
import Opaleye.TF.Expr
import Opaleye.TF.Insert
import Opaleye.TF.Interpretation
import Opaleye.TF.Machinery
import Opaleye.TF.Scope

-- | Indicate whether or not a column can take null values.
data PGNull t
  = NotNullable t
  | Nullable t

-- | Given a way to interpret the underlying column type to a Haskell type,
-- we can interpret a 'PGDefault' type into a Haskell type.
type instance Col Interpret ('Nullable x) = Maybe (Col Interpret x)

data Null a = Null | NotNull a

toNullable :: Expr s a -> Expr s ('Nullable a)
toNullable (Expr a) = Expr a

null :: Expr s ('Nullable a)
null = Expr (Op.ConstExpr Op.NullLit)


type instance Col (Compose (Expr s) 'Nullable) ('HasDefault col) = Col (Compose (Expr s) 'Nullable) col
type instance Col (Compose (Expr s) 'Nullable) ('NoDefault col) = Col (Compose (Expr s) 'Nullable) col
type instance Col (Compose (Expr s) 'Nullable) ('NotNullable col) = Expr s ('Nullable col)
type instance Col (Compose (Expr s) 'Nullable) ('Nullable col) = Expr s ('Nullable col)

type instance Col (Compose Interpret 'Nullable) ('HasDefault col) = Col (Compose Interpret 'Nullable) col
type instance Col (Compose Interpret 'Nullable) ('NoDefault col) = Col (Compose Interpret 'Nullable) col
type instance Col (Compose Interpret 'Nullable) ('NotNullable col) = Col Interpret ('Nullable col)
type instance Col (Compose Interpret 'Nullable) ('Nullable col) = Col Interpret ('Nullable col)
type instance Col (Expr s) ('NotNullable col) = Col (Expr s) col
type instance Col (Expr s) ('Nullable col) = Expr s ('Nullable col)
type instance Col Insertion (col :: PGNull k) = Col (Expr 'Z) col
type instance Col InsertionWithDefault (col :: PGNull k) = Default (Col (Expr 'Z) col)
type instance Col Interpret ('NotNullable col) = Col Interpret col
type instance Col Interpret ('Nullable col) = Maybe (Col Interpret col)

-- | Eliminate 'PGNull' from the type of an 'Expr'. Like 'maybe' for Haskell
-- values.
nullable
  :: Expr s b -> (Expr s a -> Expr s b) -> Expr s ('Nullable a) -> Expr s b
nullable (Expr a) f (Expr e) =
  case Op.matchNullable
         (Op.Column a)
         (\(Op.Column x) ->
            case f (Expr x) of
              Expr x' -> Op.Column x')
         (Op.Column e) of
    Op.Column b -> Expr b
