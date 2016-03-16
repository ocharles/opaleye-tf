{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Expr where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op

newtype Expr (columnType :: k) = Expr Op.PrimExpr
