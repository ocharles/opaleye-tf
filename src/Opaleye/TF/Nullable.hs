{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Nullable where

import Opaleye.TF.Expr
import Opaleye.TF.Interpretation
import Opaleye.TF.Machinery
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op

-- | Indicate whether or not a column can take null values.
data PGNull t
  = NotNullable t
  | Nullable t

-- | Given a way to interpret the underlying column type to a Haskell type,
-- we can interpret a 'PGDefault' type into a Haskell type.
data InterpretNullable (f :: TyFun (PGNull t) *)
type instance Interpretation (t :: PGNull k) = InterpretNullable

type instance Apply InterpretNullable ('NotNullable t) = Apply (Interpretation t) t
type instance Apply InterpretNullable ('Nullable t) = Maybe (Apply (Interpretation t) t)

data Null a = Null | NotNull a

nullable :: Expr a -> Expr ('Nullable a)
nullable (Expr a) = Expr a

null :: Expr ('Nullable a)
null = Expr (Op.ConstExpr Op.NullLit)
