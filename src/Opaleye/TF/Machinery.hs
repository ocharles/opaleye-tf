{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Machinery where

data TyFun :: * -> * -> *

type family Apply (f :: TyFun k1 k2 -> *) (x :: k1) :: k2

-- Polykinded compose. I think transformers might have this now.
data Compose (f :: l -> *) (g :: k -> l) (a :: k) = Compose (f (g a))
