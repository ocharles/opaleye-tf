{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Default where

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
