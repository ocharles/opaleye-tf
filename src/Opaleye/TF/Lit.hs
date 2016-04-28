{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.TF.Lit where

import Opaleye.TF.Col
import Opaleye.TF.Expr
import Opaleye.TF.Interpretation
import Opaleye.TF.Nullable
import Prelude hiding (null)

class Lit (exprType :: k) where
  lit :: Col Interpret exprType -> Expr s exprType

instance (Maybe (Col Interpret a) ~ Col Interpret ('Nullable a),Lit a) => Lit ('Nullable a) where
  lit (Just haskell) = mapExpr Cast (lit haskell :: Expr s a)
  lit Nothing = null
