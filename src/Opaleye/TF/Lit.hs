{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Lit where

import Opaleye.TF.Expr
import Opaleye.TF.Nullable
import Prelude hiding (null)

class Lit (pgType :: k) (haskellType :: *) | pgType -> haskellType where
  lit :: haskellType -> Expr pgType

litMaybe :: Lit pg haskell
         => Maybe haskell -> Expr ('Nullable pg)
litMaybe = maybe null (toNullable . lit)
