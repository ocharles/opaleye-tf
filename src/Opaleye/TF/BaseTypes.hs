{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.BaseTypes where

import Data.Int (Int32, Int64)
import Data.Text
import Data.Time (LocalTime)
import GHC.TypeLits (Nat)
import qualified Opaleye.Internal.Column as Op
import qualified Opaleye.PGTypes as Op
import Opaleye.TF.Interpretation
import Opaleye.TF.Expr
import Opaleye.TF.Lit
import Opaleye.TF.Machinery (Apply, TyFun)

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

data InterpretPGType (fun :: TyFun PGType *)
type instance Interpretation (a :: PGType) = InterpretPGType

type instance Apply InterpretPGType 'PGBigint = Int64
instance Lit 'PGBigint Int64 where
  lit = Expr . Op.unColumn . Op.pgInt8

type instance Apply InterpretPGType 'PGBoolean = Bool
instance Lit 'PGBoolean Bool where
  lit = Expr . Op.unColumn . Op.pgBool

type instance Apply InterpretPGType 'PGInteger = Int32
instance Lit 'PGInteger Int32 where
  lit = Expr . Op.unColumn . Op.pgInt4 . fromIntegral

type instance Apply InterpretPGType 'PGReal = Float
instance Lit 'PGReal Float where
  lit = Expr . Op.unColumn . Op.pgDouble . realToFrac

type instance Apply InterpretPGType 'PGText = Text
instance Lit 'PGText Text where
  lit = Expr . Op.unColumn . Op.pgStrictText

type instance Apply InterpretPGType ('PGTimestamp 'WithoutTimeZone) = LocalTime
instance Lit ('PGTimestamp 'WithoutTimeZone) LocalTime where
  lit = Expr . Op.unColumn . Op.pgLocalTime
