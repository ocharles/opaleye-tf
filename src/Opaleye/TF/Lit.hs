{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}

module Opaleye.TF.Lit where

import Opaleye.TF.Expr

class Lit (pgType :: k) (haskellType :: *) | pgType -> haskellType where
  lit :: haskellType -> Expr pgType
