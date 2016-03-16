{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.TF.Col where

import Opaleye.TF.Insert
import Opaleye.TF.Default
import Opaleye.TF.Expr
import Opaleye.TF.Machinery
import Opaleye.TF.Nullable
import Opaleye.TF.Interpretation

-- | The workhorse type family in @opaleye-tf@. This type family "collapses"
-- noisy types down into things that are more meaningful. For example, when
-- used with 'Interpret' you just get ordinary Haskell values.
type family Col (f :: a -> b) (x :: a) :: b where
  -- If we use the Interpret functor, then we'll use defunctionalisation to
  -- get down to something in *
  Col Interpret x = Apply (Interpretation x) x  -- TODO In GHC 8 I believe we can do Col Interpret (x :: k) = Apply (Interpretation k) x

  -- The Insertion functor is like Interpret, in that it maps to ordinary
  -- Haskell types. However, it also maps PGDefault to a special Maybe-like
  -- type to allow the use of 'DEFAULT'.
  Col Insertion ('HasDefault t) = Col Insertion t
  Col Insertion ('NoDefault t) = Col Insertion t
  Col Insertion ('NotNullable t) = Expr t
  Col Insertion ('Nullable t) = Expr ('Nullable t)
  Col Insertion t = Expr t

  -- InterpretNull is used for left joins.
  -- If we use InterpretNull with PGNull, then we get a Maybe of whatever t
  -- is. This avoids a double Maybe.
  Col (Compose Interpret 'Nullable) ('Nullable t) = Maybe (Col Interpret t)

  -- Otherwise, we still get a Maybe of whatever the underlying type is.
  Col (Compose Interpret 'Nullable) t = Maybe (Col Interpret t)

  -- In the process of a left join, we work under Compose Expr PGNull.
  -- This basically does the same as InterpretNull PGNull - collapsing
  -- multiple PGNull layers together.
  Col (Compose Expr 'Nullable) ('Nullable t) = Col (Compose Expr 'Nullable) t

  -- If something is marked as NotNullable but we're in the context of being Nullable, then it becomes Nullable.
  Col (Compose Expr 'Nullable) ('NotNullable t) = Col (Compose Expr 'Nullable) t

  -- If the given column isn't nullable, make it nullable.
  Col (Compose Expr 'Nullable) t = Col Expr ('Nullable t)

  -- If I don't know what f is, then just give me f x.
  Col f x = f x
