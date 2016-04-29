{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Opaleye.TF.BaseTypes where

import Data.ByteString (ByteString)
import Data.Fixed (E0, E1, E2, E3, E6, E9, Fixed)
import Data.Int (Int32, Int64)
import Data.Text
import Data.Time (LocalTime, UTCTime)
import GHC.TypeLits (Nat)
import qualified Opaleye.Internal.Column as Op
import qualified Opaleye.PGTypes as Op
import Opaleye.TF.Col
import Opaleye.TF.Expr
import Opaleye.TF.Interpretation
import Opaleye.TF.Lit
import Opaleye.TF.Nullable
import Opaleye.TF.Machinery

data WithTimeZone
  = WithTimeZone
  | WithoutTimeZone
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

-- | The universe of base types known about by PostgreSQL.
data PGType
  = PGBigint                 -- ^ @bigint@
  | PGBigserial              -- ^ @bigserial@
  | PGBit (Maybe Nat)        -- ^ @bit [ (n) ]@
  | PGBitVarying (Maybe Nat) -- ^ @bit varying [ (n) ]@
  | PGBoolean                -- ^ @boolean@
  | PGBox                    -- ^ @box@
  | PGBytea                  -- ^ @bytea@
  | PGCharacter (Maybe Nat)  -- ^ @character [ (n) ]@
  | PGVarchar Nat            -- ^ @character varying (n)@ (unbound @character varying@ is 'PGText')
  | PGCidr                   -- ^ @cidr@
  | PGCircle                 -- ^ @circle@
  | PGDate                   -- ^ @date@
  | PGDouble                 -- ^ @double precision@
  | PGInet                   -- ^ @inet@
  | PGInteger                -- ^ @integer@
  | PGInterval               -- ^ @interval@. There is no ability to specify the precision or fields, if you require this please open a feature request.
  | PGJSON                   -- ^ @json@
  | PGJSONB                  -- ^ @jsonb@
  | PGLine                   -- ^ @line@
  | PGLseg                   -- ^ @lseg@
  | PGMacaddr                -- ^ @macaddr@
  | PGMoney                  -- ^ @money@
  | PGNumeric Nat
              Nat            -- ^ @numeric(p,s)@
  | PGPath                   -- ^ @path@
  | PGPGLSN                  -- ^ @pg_lsn@
  | PGPoint                  -- ^ @point@
  | PGPolygon                -- ^ @polygon@
  | PGReal                   -- ^ @real@
  | PGSmallint               -- ^ @smallint@
  | PGSmallserial            -- ^ @smallserial@
  | PGSerial                 -- ^ @serial@
  | PGText                   -- ^ @text@ and @character varying@
  | PGTime WithTimeZone      -- ^ @time with/without time zone@
  | PGTimestamp WithTimeZone -- ^ @timestamp with/without time zone@
  | PGTSQuery                -- ^ @tsquery@
  | PGTSVector               -- ^ @tsvector@
  | PGTXIDSnapshot           -- ^ @txid_sapnshot@
  | PGUUID                   -- ^ @uuid@
  | PGXML                    -- ^ @xml@

type instance Col (Expr s) (t :: PGType) = Expr s t
type instance Col (Compose (Expr s) 'Nullable) (a :: PGType) = Expr s ('Nullable a)
type instance Col (Compose (Expr s) 'NotNullable) (a :: PGType) = Expr s a

type instance Col Interpret 'PGBigint = Int64
type instance Col Interpret 'PGBoolean = Bool
type instance Col Interpret 'PGInteger = Int32
type instance Col Interpret 'PGReal = Float
type instance Col Interpret 'PGText = Text
type instance Col Interpret ('PGTimestamp 'WithoutTimeZone) = LocalTime
type instance Col Interpret ('PGTimestamp 'WithTimeZone) = UTCTime
type instance Col Interpret 'PGDouble = Double
type instance Col Interpret ('PGNumeric p 0) = Fixed E0
type instance Col Interpret ('PGNumeric p 1) = Fixed E1
type instance Col Interpret ('PGNumeric p 2) = Fixed E2
type instance Col Interpret ('PGNumeric p 3) = Fixed E3
type instance Col Interpret ('PGNumeric p 6) = Fixed E6
type instance Col Interpret ('PGNumeric p 9) = Fixed E9
type instance Col Interpret 'PGBytea = ByteString

instance Lit 'PGBigint where
  lit = Expr . Op.unColumn . Op.pgInt8

instance Lit 'PGBoolean where
  lit = Expr . Op.unColumn . Op.pgBool

instance Lit 'PGInteger where
  lit = Expr . Op.unColumn . Op.pgInt4 . fromIntegral

instance Lit 'PGReal where
  lit = Expr . Op.unColumn . Op.pgDouble . realToFrac

instance Lit 'PGText where
  lit = Expr . Op.unColumn . Op.pgStrictText

instance Lit ('PGTimestamp 'WithoutTimeZone) where
  lit = Expr . Op.unColumn . Op.pgLocalTime

instance Lit ('PGTimestamp 'WithTimeZone) where
  lit = Expr . Op.unColumn . Op.pgUTCTime

instance Lit 'PGDouble where
  lit = Expr . Op.unColumn . Op.pgDouble

pgNow :: Expr s ('PGTimestamp 'WithTimeZone)
pgNow = mapExpr Cast (lit (pack "now") :: Expr s 'PGText)
