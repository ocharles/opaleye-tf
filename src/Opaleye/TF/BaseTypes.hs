{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.BaseTypes where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text
import Data.Time (LocalTime)
import GHC.TypeLits (Nat)
import qualified Opaleye.Internal.Column as Op
import qualified Opaleye.PGTypes as Op
import Opaleye.TF.Col
import Opaleye.TF.Expr
import Opaleye.TF.Interpretation
import Opaleye.TF.Lit
import Opaleye.TF.Nullable

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

type instance Col Interpret 'PGBigint = Int64
instance Lit 'PGBigint Int64 where
  lit = Expr . Op.unColumn . Op.pgInt8

type instance Col Interpret 'PGBoolean = Bool
instance Lit 'PGBoolean Bool where
  lit = Expr . Op.unColumn . Op.pgBool

type instance Col Interpret 'PGInteger = Int32
instance Lit 'PGInteger Int32 where
  lit = Expr . Op.unColumn . Op.pgInt4 . fromIntegral

type instance Col Interpret 'PGReal = Float
instance Lit 'PGReal Float where
  lit = Expr . Op.unColumn . Op.pgDouble . realToFrac

type instance Col Interpret 'PGText = Text
instance Lit 'PGText Text where
  lit = Expr . Op.unColumn . Op.pgStrictText

type instance Col Interpret ('PGTimestamp 'WithoutTimeZone) = LocalTime
instance Lit ('PGTimestamp 'WithoutTimeZone) LocalTime where
  lit = Expr . Op.unColumn . Op.pgLocalTime

type instance Col Interpret 'PGDouble = Double
instance Lit 'PGDouble Double where
  lit = Expr . Op.unColumn . Op.pgDouble

type instance Col Interpret 'PGBytea = ByteString

type instance Col Expr (t :: PGType) = Expr t
type instance Col NullableExpr (t :: PGType) = Expr ('Nullable t)

pgNow :: Expr ('PGTimestamp withOrWithoutTimezone)
pgNow =
  case lit (pack "now") :: Expr 'PGText of
    Expr a -> Expr a
