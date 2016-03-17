{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.TF.Col where

-- | The workhorse type family in @opaleye-tf@. This type family "collapses"
-- noisy types down into things that are more meaningful. For example, when
-- used with 'Interpret' you just get ordinary Haskell values.
type family Col (f :: a -> b) (x :: a) :: b
