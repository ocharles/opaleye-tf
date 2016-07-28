-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TypeFamilies #-}

module Opaleye.TF.Text where

import qualified Opaleye.Internal.Column              as Op (Column (..), binOp)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op
import           Opaleye.TF.BaseTypes
import           Opaleye.TF.Expr

-- * Regular expression operators

-- See https://www.postgresql.org/docs/9.5/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP

-- | Matches regular expression, case sensitive
(~.) :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGBoolean
Expr a ~. Expr b =
  case Op.binOp (Op.OpOther "~.") (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | Matches regular expression, case insensitive
(~*) :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGBoolean
Expr a ~* Expr b =
  case Op.binOp (Op.OpOther "~*") (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | Does not match regular expression, case sensitive
(!~) :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGBoolean
Expr a !~ Expr b =
  case Op.binOp (Op.OpOther "!~") (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | Does not match regular expression, case insensitive
(!~*) :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGBoolean
Expr a !~* Expr b =
  case Op.binOp (Op.OpOther "!~*") (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- See https://www.postgresql.org/docs/9.5/static/functions-Expr s.'PGHtml

-- * Standard SQL functions

bitLength :: Expr s 'PGText -> Expr s 'PGInteger
bitLength (Expr a) = Expr (Op.FunExpr "bit_length" [a])

charLength :: Expr s 'PGText -> Expr s 'PGInteger
charLength (Expr a) = Expr (Op.FunExpr "char_length" [a])

lower :: Expr s 'PGText -> Expr s 'PGText
lower (Expr a) = Expr (Op.FunExpr "lower" [a])

octetLength :: Expr s 'PGText -> Expr s 'PGInteger
octetLength (Expr a) = Expr (Op.FunExpr "octet_length" [a])

upper :: Expr s 'PGText -> Expr s 'PGText
upper (Expr a) = Expr (Op.FunExpr "upper" [a])

-- * PostgreSQL functions

ascii :: Expr s 'PGText -> Expr s 'PGInteger
ascii (Expr a) = Expr (Op.FunExpr "ascii" [a])

btrim :: Expr s 'PGText -> Maybe (Expr s 'PGText) -> Expr s 'PGText
btrim (Expr a) mb = Expr (Op.FunExpr "btrim" ([a] ++ maybe [] (\(Expr x) -> [x]) mb))

chr :: Expr s 'PGInteger -> Expr s 'PGText
chr (Expr a) = Expr (Op.FunExpr "chr" [a])

-- concat :: NonEmptyList (AnyExpr s) -> Expr s 'PGText
-- concat = _

-- concatWs :: Expr s 'PGText -> NonEmptyList (AnyExpr s) -> Expr s 'PGText
-- concatWs = _

convert :: Expr s 'PGBytea -> Expr s name -> Expr s name -> Expr s 'PGBytea
convert (Expr a) (Expr b) (Expr c) = Expr (Op.FunExpr "convert" [a,b,c])

convertFrom :: Expr s 'PGBytea -> Expr s name -> Expr s 'PGText
convertFrom (Expr a) (Expr b) = Expr (Op.FunExpr "convert_from" [a,b])

convertTo :: Expr s 'PGText -> Expr s name -> Expr s 'PGBytea
convertTo (Expr a) (Expr b) = Expr (Op.FunExpr "convert_to" [a,b])

decode :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGBytea
decode (Expr a) (Expr b) = Expr (Op.FunExpr "decode" [a,b])

encode :: Expr s 'PGBytea -> Expr s 'PGText -> Expr s 'PGText
encode (Expr a) (Expr b) = Expr (Op.FunExpr "encode" [a,b])

-- format :: Expr s 'PGText -> NonEmptyList (AnyExpr s) -> Expr s 'PGText
-- format = _

initcap :: Expr s 'PGText -> Expr s 'PGText
initcap (Expr a) = Expr (Op.FunExpr "initcap" [a])

left :: Expr s 'PGText -> Expr s 'PGInteger -> Expr s 'PGText
left (Expr a) (Expr b) = Expr (Op.FunExpr "left" [a, b])

length :: Expr s 'PGText -> Expr s 'PGInteger
length (Expr a) = Expr (Op.FunExpr "length" [a])

lengthEncoding :: Expr s 'PGBytea -> Expr s name -> Expr s 'PGInteger
lengthEncoding (Expr a) (Expr b) = Expr (Op.FunExpr "length" [a,b])

lpad :: Expr s 'PGText -> Expr s 'PGInteger -> Maybe (Expr s 'PGText) -> Expr s 'PGText
lpad (Expr a) (Expr b) mc = Expr (Op.FunExpr "lpad" ([a,b] ++ maybe [] (\(Expr x) -> [x]) mc))

ltrim :: Expr s 'PGText -> Maybe (Expr s 'PGText) -> Expr s 'PGText
ltrim (Expr a) mb = Expr (Op.FunExpr "ltrim" ([a] ++ maybe [] (\(Expr x) -> [x]) mb))

md5 :: Expr s 'PGText -> Expr s 'PGText
md5 (Expr a) = Expr (Op.FunExpr "md5" [a])

pgClientEncoding :: Expr s name
pgClientEncoding = Expr (Op.FunExpr "pg_client_encoding" [])

quoteIdent :: Expr s 'PGText -> Expr s 'PGText
quoteIdent (Expr a) = Expr (Op.FunExpr "quote_ident" [a])

quoteLiteral :: Expr s 'PGText -> Expr s 'PGText
quoteLiteral (Expr a) = Expr (Op.FunExpr "quote_literal" [a])
quoteLiteralCoerce :: Expr s a -> Expr s 'PGText
quoteLiteralCoerce (Expr a) = Expr (Op.FunExpr "quote_literal_coerce" [a])

quoteNullable :: Expr s 'PGText -> Expr s 'PGText
quoteNullable (Expr a) = Expr (Op.FunExpr "quote_nullable" [a])

quoteNullableCoerce :: Expr s a -> Expr s 'PGText
quoteNullableCoerce (Expr a) = Expr (Op.FunExpr "quote_nullable_coerce" [a])

-- regexpMatches :: Expr s 'PGText -> Expr s 'PGText -> Maybe (Expr s 'PGText) -> Expr s (setof 'PGText)
-- regexpMatches (Expr a) (Expr b) mc = Expr (Op.FunExpr "regexp_matches" ([a,b] ++ maybe [] (\(Expr x) -> [x]) mc))

regexpReplace :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGText -> Maybe (Expr s 'PGText) -> Expr s 'PGText
regexpReplace (Expr a) (Expr b) (Expr c) md = Expr (Op.FunExpr "regexp_replace" ([a,b,c] ++ maybe [] (\(Expr x) -> [x]) md))

regexpSplitToArray :: Expr s 'PGText -> Expr s 'PGText -> Maybe (Expr s 'PGText) -> Expr s 'PGText
regexpSplitToArray (Expr a) (Expr b) mc = Expr (Op.FunExpr "regexp_split_to_array" ([a,b] ++ maybe [] (\(Expr x) -> [x]) mc))

-- regexpSplitToTable :: Expr s 'PGText -> Expr s 'PGText -> Maybe (Expr s 'PGText) -> Expr s (setof 'PGText)
-- regexpSplitToTable (Expr a) (Expr b) mc = Expr (Op.FunExpr "regexp_split_to_table" [a,b,c])

repeat :: Expr s 'PGText -> Expr s 'PGInteger -> Expr s 'PGText
repeat (Expr a) (Expr b) = Expr (Op.FunExpr "repeat" [a,b])

replace :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGText
replace (Expr a) (Expr b) (Expr c) = Expr (Op.FunExpr "replace" [a,b,c])

reverse :: Expr s 'PGText -> Expr s 'PGText
reverse (Expr a) = Expr (Op.FunExpr "reverse" [a])

right :: Expr s 'PGText -> Expr s 'PGInteger -> Expr s 'PGText
right (Expr a) (Expr b) = Expr (Op.FunExpr "right" [a, b])

rpad :: Expr s 'PGText -> Expr s 'PGInteger -> Maybe (Expr s 'PGText) -> Expr s 'PGText
rpad (Expr a) (Expr b) mc = Expr (Op.FunExpr "rpad" ([a,b] ++ maybe [] (\(Expr x) -> [x]) mc))

rtrim :: Expr s 'PGText -> Maybe (Expr s 'PGText) -> Expr s 'PGText
rtrim (Expr a) mb = Expr (Op.FunExpr "rtrim" ([a] ++ maybe [] (\(Expr x) -> [x]) mb))

splitPart :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGInteger -> Expr s 'PGText
splitPart (Expr a) (Expr b) (Expr c) = Expr (Op.FunExpr "split_part" [a,b,c])

strpos :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGInteger
strpos (Expr a) (Expr b) = Expr (Op.FunExpr "strpos" [a,b])

substr :: Expr s 'PGText -> Expr s 'PGInteger -> Maybe (Expr s 'PGInteger) -> Expr s 'PGText
substr (Expr str) (Expr from) mcount = Expr (Op.FunExpr "substr" ([str,from] ++ maybe [] (\(Expr x) -> [x]) mcount))

toAscii :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGText
toAscii (Expr a) (Expr b) = Expr (Op.FunExpr "toAscii" [a,b])

toHex :: Expr s 'PGInteger -> Expr s 'PGText
toHex (Expr a) = Expr (Op.FunExpr "toHex" [a])

translate :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGText
translate (Expr a) (Expr b) (Expr c) = Expr (Op.FunExpr "translate" [a,b,c])

