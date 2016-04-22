{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Default where

import Opaleye.TF.Col
import Opaleye.TF.Expr
import Opaleye.TF.Insert
import Opaleye.TF.Interpretation

-- | Indicate whether or not a column has a default value.
data PGDefault t
  = HasDefault t
  | NoDefault t

type instance Col Expr ('HasDefault col) = Col Expr col
type instance Col Expr ('NoDefault col) = Col Expr col
type instance Col Interpret ('HasDefault col) = Col Interpret col
type instance Col Interpret ('NoDefault col) = Col Interpret col
type instance Col Insertion ('HasDefault col) = Col InsertionWithDefault col
type instance Col Insertion ('NoDefault col) = Col Insertion col

data InsertionWithDefault (col :: k)

-- | 'Default' is like 'Maybe', but only has meaning in @INSERT@ statements.
data Default a
  = InsertDefault -- ^ Use the @DEFAULT@ value for this column.
  | ProvideValue a -- ^ Override the default value by providing an explicit value.

insertDefault :: Default a
insertDefault = InsertDefault

overrideDefault :: a -> Default a
overrideDefault = ProvideValue
