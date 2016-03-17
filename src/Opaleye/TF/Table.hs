{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Table where

import Opaleye.TF.Col
import Opaleye.TF.Expr
import Opaleye.TF.Insert
import Opaleye.TF.Interpretation
import GHC.TypeLits (Symbol)
import Data.Proxy

type family TableName (t :: k) :: Symbol

data Column a = Column Symbol a

data ExtractSchema (col :: k)

type instance Col ExtractSchema ('Column columnName col) = Proxy columnName
type instance Col Expr ('Column name x) = Col Expr x
type instance Col Interpret ('Column name x) = Col Interpret x
type instance Col Insertion ('Column name x) = Col Insertion x
