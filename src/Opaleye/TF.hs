{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}

module Opaleye.TF
       ( -- $intro
         -- * The 'Col' type family
         type Col,

         -- * Mapping PostgreSQL types
         PGType(..), Lit(..), null, toNullable, pgNow, mapExpr,

         -- * Defining tables
         ExtractSchema, TableName, Column(..), PGNull(..), PGDefault(..),

         -- * Querying tables
         queryTable, queryBy, queryOnto, Expr, select, leftJoin, restrict, (==.), (/=.), (<.), (<=.), (>.), (>=.),(||.), ilike, isNull, not,
         filterQuery, asc, desc, orderNulls, OrderNulls(..), orderBy, Op.limit, Op.offset,

         -- * Inserting data
         insert, insert1Returning, Insertion, Default(..), overrideDefault, insertDefault,

         -- * TODO Organize
         Op.Query, Op.QueryArr,

         -- * Implementation details
         Compose(..), Interpret, Selectable, Insertable, ColumnView, queryRunner

         )
       where

import Control.Applicative
import Control.Arrow (first, (&&&), returnA)
import Control.Category ((.), id)
import Control.Monad (void)
import Data.Int
import Data.Profunctor
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import GHC.Generics
import GHC.TypeLits
import qualified Opaleye.Column as Op hiding (toNullable)
import qualified Opaleye.Internal.Column as Op
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op
import qualified Opaleye.Internal.Join as Op
import qualified Opaleye.Internal.Order as Op
import qualified Opaleye.Internal.PackMap as Op
#if __GLASGOW_HASKELL__ >= 800
import qualified Opaleye.Internal.PrimQuery as Op (PrimQuery' (Join), JoinType (LeftJoin))
#else
import qualified Opaleye.Internal.PrimQuery as Op (PrimQuery (Join), JoinType (LeftJoin))
#endif
import qualified Opaleye.Internal.RunQuery as Op
import qualified Opaleye.Internal.Table as Op
import qualified Opaleye.Internal.TableMaker as Op
import qualified Opaleye.Internal.Tag as Op
import qualified Opaleye.Internal.Unpackspec as Op
import qualified Opaleye.Internal.QueryArr as Op
import qualified Opaleye.Manipulation as Op
import qualified Opaleye.Operators as Op
import qualified Opaleye.Order as Op
import qualified Opaleye.RunQuery as Op
import Opaleye.TF.BaseTypes
import Opaleye.TF.Col
import Opaleye.TF.Default
import Opaleye.TF.Expr
import Opaleye.TF.Insert
import Opaleye.TF.Interpretation
import Opaleye.TF.Lit
import Opaleye.TF.Machinery
import Opaleye.TF.Nullable
import Opaleye.TF.Table
import qualified Opaleye.Table as Op hiding (required)
import Prelude hiding (null, (.), id, not)

--------------------------------------------------------------------------------

-- | 'queryTable' moves from 'Table' to 'Expr'. Accessing the fields of your
-- table record will now give you expressions to view data in the individual
-- columns.
queryTable :: forall (rel :: (k -> *) -> *).
              (Generic (rel ExtractSchema),Generic (rel Expr), InjPackMap (Rep (rel Expr)), ColumnView (Rep (rel ExtractSchema)) (Rep (rel Expr)), KnownSymbol (TableName rel))
           => Op.Query (rel Expr)
queryTable =
  Op.queryTableExplicit
    columnMaker
    (Op.Table (symbolVal (Proxy :: Proxy (TableName rel)))
              (Op.TableProperties undefined
                                  (Op.View columnView)))
  where columnView = columnViewRep (Proxy :: Proxy (Rep (rel ExtractSchema)))
        columnMaker =
          Op.ColumnMaker
            (Op.PackMap (\inj columns -> fmap to (injPackMap inj columns)))

-- | Generate an @INNER JOIN@ directly from a field accessor.
-- This has a similar function to 'queryTable' but with has an input specified, which makes composition of joins easier.
--
-- @
--     listPackagesByUserName :: Text -> Query (Package Expr)
--     listPackagesByUserName = queryBy packageMaintainerId . queryBy userName
-- @
--
-- This function can be thought of as a sort of reverse form of 'arr', where the input and output in the resulting 'QueryArr' is swapped around.
-- However, note that @queryBy f@ is not inverse to @arr f@ since neither
queryBy :: forall (rel :: (k -> *) -> *) prim. (Generic (rel Expr), Generic (rel ExtractSchema), InjPackMap (Rep (rel Expr)), ColumnView (Rep (rel ExtractSchema)) (Rep (rel Expr)), KnownSymbol (TableName rel))
        => (rel Expr -> Expr prim) -> Op.QueryArr (Expr prim) (rel Expr)
queryBy = queryOnto (==.)

-- | A generalization of 'queryBy' taking an arbitrary comparison operator for the join clause.
queryOnto :: forall (rel :: (k -> *) -> *) prim. (Generic (rel Expr), Generic (rel ExtractSchema), InjPackMap (Rep (rel Expr)), ColumnView (Rep (rel ExtractSchema)) (Rep (rel Expr)), KnownSymbol (TableName rel))
          => (Expr prim -> Expr prim -> Expr 'PGBoolean)
          -> (rel Expr -> Expr prim)
          -> Op.QueryArr (Expr prim) (rel Expr)
queryOnto op f = proc a -> do
  t <- queryTable -< ()
  restrict -< f t `op` a
  returnA -< t

-- | A type class to get the column names out of the record.
class ColumnView f g where
  columnViewRep :: proxy f -> g x

instance ColumnView f f' => ColumnView (M1 i c f) (M1 i c f') where
  columnViewRep _ = M1 (columnViewRep (Proxy :: Proxy f))

instance (ColumnView f f', ColumnView g g') => ColumnView (f :*: g) (f' :*: g') where
  columnViewRep _ = columnViewRep (Proxy :: Proxy f) :*: columnViewRep (Proxy :: Proxy g)

instance KnownSymbol columnName => ColumnView (K1 i (Proxy columnName)) (K1 i (Expr colType)) where
  columnViewRep _ = K1 (Expr (Op.BaseTableAttrExpr (symbolVal (Proxy :: Proxy columnName))))

-- A type to generate that weird PackMap thing queryTableExplicit wants.
-- Basically associating column symbol names with the given record.
class InjPackMap g where
  injPackMap :: Applicative f => (Op.PrimExpr -> f Op.PrimExpr) -> g x -> f (g x)

instance InjPackMap f => InjPackMap (M1 i c f) where
  injPackMap f (M1 a) = fmap M1 (injPackMap f a)

instance (InjPackMap f, InjPackMap g) => InjPackMap (f :*: g) where
  injPackMap f (a :*: b) = liftA2 (:*:) (injPackMap f a) (injPackMap f b)

instance InjPackMap (K1 i (Expr colType)) where
  injPackMap f (K1 (Expr prim)) = fmap (K1 . Expr) (f prim)

--------------------------------------------------------------------------------

-- | 'select' executes a PostgreSQL query as a @SELECT@ statement, returning
-- data mapped to Haskell values.
select :: Selectable pg haskell => PG.Connection -> Op.Query pg -> IO [haskell]
select conn = Op.runQueryExplicit queryRunner conn

-- A type class for selectable things, so we can return a table or a tuple.
class Selectable expr haskell | expr -> haskell where
  queryRunner :: Op.QueryRunner expr haskell

instance (haskell ~ Col Interpret t, PG.FromField haskell) => Selectable (Expr t) haskell where
  queryRunner = lmap (\(Expr a) -> Op.Column a) (Op.queryRunner Op.fieldQueryRunnerColumn)

-- A scary instance for interpreting Expr to Haskell types generically.
instance (Generic (rel Expr),ParseRelRep (Rep (rel Expr)) (Rep (rel Interpret)),Generic (rel Interpret),UnpackspecRel (Rep (rel Expr)), HasFields (Rep (rel Expr))) =>
           Selectable (rel Expr) (rel Interpret) where
  queryRunner = gqueryRunner

-- The same instance but for left joins.
instance (Generic (rel (Compose Expr 'Nullable)),ParseRelRep (Rep (rel (Compose Expr 'Nullable))) (Rep (rel (Compose Interpret 'Nullable))),Generic (rel (Compose Interpret 'Nullable)),UnpackspecRel (Rep (rel (Compose Expr 'Nullable))), Generic (rel Interpret), DistributeMaybe (rel (Compose Interpret 'Nullable)) (rel Interpret), HasFields (Rep (rel (Compose Expr 'Nullable)))) =>
           Selectable (rel (Compose Expr 'Nullable)) (Maybe (rel Interpret)) where
  queryRunner = fmap distributeMaybe (gqueryRunner :: Op.QueryRunner (rel (Compose Expr 'Nullable)) (rel (Compose Interpret 'Nullable)))

-- This lets us turn a record of Maybe's into a Maybe of fields.
class DistributeMaybe x y | x -> y where
  distributeMaybe :: x -> Maybe y

instance (Generic (rel (Compose Interpret 'Nullable)), Generic (rel Interpret), GDistributeMaybe (Rep (rel (Compose Interpret 'Nullable))) (Rep (rel Interpret))) => DistributeMaybe (rel (Compose Interpret 'Nullable)) (rel Interpret) where
  distributeMaybe = fmap to . gdistributeMaybe . from

class GDistributeMaybe x y where
  gdistributeMaybe :: x a -> Maybe (y a)

instance GDistributeMaybe f f' => GDistributeMaybe (M1 i c f) (M1 i c f') where
  gdistributeMaybe (M1 a) = fmap M1 (gdistributeMaybe a)

instance (GDistributeMaybe f f', GDistributeMaybe g g') => GDistributeMaybe (f :*: g) (f' :*: g') where
  gdistributeMaybe (a :*: b) = liftA2 (:*:) (gdistributeMaybe a) (gdistributeMaybe b)

instance GDistributeMaybe (K1 i (Maybe a)) (K1 i a) where
  gdistributeMaybe (K1 a) = fmap K1 a

instance GDistributeMaybe (K1 i (Maybe a)) (K1 i (Maybe a)) where
  gdistributeMaybe (K1 a) = fmap (K1 . Just) a

-- Tuples
instance (Selectable e1 h1, Selectable e2 h2) => Selectable (e1,e2) (h1,h2) where
  queryRunner = queryRunner ***! queryRunner

instance (Selectable e1 h1,Selectable e2 h2,Selectable e3 h3) => Selectable (e1,e2,e3) (h1,h2,h3) where
  queryRunner =
    dimap (\(a,b,c) -> ((a,b),c))
          (\((a,b),c) -> (a,b,c))
          queryRunner

instance (Selectable e1 h1,Selectable e2 h2,Selectable e3 h3,Selectable e4 h4) => Selectable (e1,e2,e3,e4) (h1,h2,h3,h4) where
  queryRunner =
    dimap (\(a,b,c,d) -> ((a,b),(c,d)))
          (\((a,b),(c,d)) -> (a,b,c,d))
          queryRunner

instance (Selectable e1 h1,Selectable e2 h2,Selectable e3 h3,Selectable e4 h4,Selectable e5 h5) => Selectable (e1,e2,e3,e4,e5) (h1,h2,h3,h4,h5) where
  queryRunner =
    dimap (\(a,b,c,d,e) -> ((a,b,c,d),e))
          (\((a,b,c,d),e) -> (a,b,c,d,e))
          queryRunner

instance (Selectable e1 h1,Selectable e2 h2,Selectable e3 h3,Selectable e4 h4,Selectable e5 h5,Selectable e6 h6) => Selectable (e1,e2,e3,e4,e5,e6) (h1,h2,h3,h4,h5,h6) where
  queryRunner =
    dimap (\(a,b,c,d,e,f) -> ((a,b,c,d),(e,f)))
          (\((a,b,c,d),(e,f)) -> (a,b,c,d,e,f))
          queryRunner

-- Build a query runner generically.
gqueryRunner :: (HasFields (Rep expr), Generic expr, ParseRelRep (Rep expr) (Rep haskell), Generic haskell, UnpackspecRel (Rep expr)) => Op.QueryRunner expr haskell
gqueryRunner =
  Op.QueryRunner
    (Op.Unpackspec
       (Op.PackMap
          (\inj p -> unpackRel inj (from p))))
    (fmap to . parseRelRep . from)
    (ghasFields . from)

class HasFields f where
  ghasFields :: f a -> Bool

instance HasFields (M1 i c f) where
  ghasFields _ = True

class ParseRelRep f g where
  parseRelRep :: f x -> PG.RowParser (g x)

instance ParseRelRep f f' => ParseRelRep (M1 i c f) (M1 i c f') where
  parseRelRep (M1 a) = fmap M1 (parseRelRep a)

instance (ParseRelRep f f', ParseRelRep g g') => ParseRelRep (f :*: g) (f' :*: g') where
  parseRelRep (a :*: b) = liftA2 (:*:) (parseRelRep a) (parseRelRep b)

instance (haskell ~ Col Interpret colType, PG.FromField haskell) => ParseRelRep (K1 i (Expr colType)) (K1 i haskell) where
  parseRelRep (K1 _) = fmap K1 PG.field

class UnpackspecRel rep where
  unpackRel :: Applicative f => (Op.PrimExpr -> f Op.PrimExpr) -> rep x -> f ()

instance UnpackspecRel f => UnpackspecRel (M1 i c f) where
  unpackRel inj (M1 a) = unpackRel inj a

instance (UnpackspecRel a, UnpackspecRel b) => UnpackspecRel (a :*: b) where
  unpackRel inj (a :*: b) = unpackRel inj a *> unpackRel inj b

instance UnpackspecRel (K1 i (Expr colType)) where
  unpackRel inj (K1 (Expr prim)) = void (inj prim)

--------------------------------------------------------------------------------

-- | A left join takes two queries and performs a relational @LEFT JOIN@ between
-- them. The left join has all rows of the left query, joined against zero or
-- more rows in the right query. If the join matches no rows in the right table,
-- then all columns will be @null@.
leftJoin :: (ToNull right nullRight)
         => (left -> right -> Expr 'PGBoolean)
         -> Op.QueryArr a left
         -> Op.Query right
         -> Op.QueryArr a (left,nullRight)
leftJoin f l r =
  leftJoinExplicit
    (Op.NullMaker toNull)
    l
    r
    (\(l',r') ->
       case f l' r' of
         Expr prim -> Op.Column prim)
  where
    -- This is similar to Op.leftJoinExplicit, but the resulting arrows may receive an input which is applied to the left-hand side query
    leftJoinExplicit :: Op.NullMaker columnsB nullableColumnsB
                     -> Op.QueryArr a columnsA
                     -> Op.Query columnsB
                     -> ((columnsA, columnsB) -> Op.Column _PGBool)
                     -> Op.QueryArr a (columnsA, nullableColumnsB)
    leftJoinExplicit nullmaker qA qB cond = Op.simpleQueryArr q
      where
        q (a, startTag) = ((columnsA, nullableColumnsB), primQueryR, Op.next endTag)
          where
            (columnsA, primQueryA, midTag) = Op.runSimpleQueryArr qA (a, startTag)
            (columnsB, primQueryB, endTag) = Op.runSimpleQueryArr qB ((), midTag)

            nullableColumnsB = Op.toNullable nullmaker columnsB

            Op.Column cond' = cond (columnsA, columnsB)
            primQueryR = Op.Join Op.LeftJoin cond' primQueryA primQueryB

class ToNull expr exprNull | expr -> exprNull where
  toNull :: expr -> exprNull

instance (GToNull (Rep (rel Expr)) (Rep (rel (Compose Expr 'Nullable))), Generic (rel Expr), Generic (rel (Compose Expr 'Nullable))) => ToNull (rel Expr) (rel (Compose Expr 'Nullable)) where
  toNull = to . gtoNull . from

class GToNull f g | f -> g where
  gtoNull :: f x -> g x

instance GToNull f f' => GToNull (M1 i c f) (M1 i c f') where
  gtoNull (M1 f) = M1 (gtoNull f)

instance (GToNull f f', GToNull g g') => GToNull (f :*: g) (f' :*: g') where
  gtoNull (f :*: g) = gtoNull f :*: gtoNull g

instance (t' ~ Col (Compose Expr 'Nullable) t, t' ~ Expr x) => GToNull (K1 i (Expr t)) (K1 i t') where
  gtoNull (K1 (Expr prim)) = K1 (Expr prim)

--------------------------------------------------------------------------------

-- | Apply a @WHERE@ restriction to a table.
restrict :: Op.QueryArr (Expr 'PGBoolean) ()
restrict = lmap (\(Expr prim) -> Op.Column prim) Op.restrict

-- | The PostgreSQL @=@ operator.
(==.) :: Expr a -> Expr a -> Expr 'PGBoolean
Expr a ==. Expr b =
  case Op.Column a Op..== Op.Column b of
    Op.Column c -> Expr c

-- | The PostgreSQL @!=@ or @<>@ operator.
(/=.) :: Expr a -> Expr a -> Expr 'PGBoolean
Expr a /=. Expr b =
  case Op.Column a Op../= Op.Column b of
    Op.Column c -> Expr c

-- | The PostgreSQL @OR@ operator.
(||.) :: Expr a -> Expr a -> Expr 'PGBoolean
Expr a ||. Expr b =
  case Op.Column a Op..|| Op.Column b of
    Op.Column c -> Expr c

-- | The PostgreSQL @<@ operator.
(<.) :: forall (a :: PGType). Expr a -> Expr a -> Expr 'PGBoolean
Expr a <. Expr b =
  case Op.binOp Op.OpLt (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | The PostgreSQL @<=@ operator.
(<=.) :: forall (a :: PGType). Expr a -> Expr a -> Expr 'PGBoolean
Expr a <=. Expr b =
  case Op.binOp Op.OpLtEq (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | The PostgreSQL @>@ operator.
(>.) :: forall (a :: PGType). Expr a -> Expr a -> Expr 'PGBoolean
Expr a >. Expr b =
  case Op.binOp Op.OpGt (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | The PostgreSQL @>@ operator.
(>=.) :: forall (a :: PGType). Expr a -> Expr a -> Expr 'PGBoolean
Expr a >=. Expr b =
  case Op.binOp Op.OpGtEq (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | The PostgreSQL @ILIKE@ operator.
ilike :: Expr 'PGText -> Expr 'PGText -> Expr 'PGBoolean
Expr a `ilike` Expr b =
  case Op.binOp (Op.OpOther "ILIKE") (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | The PostgreSQL @IS NULL@ operator
isNull :: Expr ('Nullable a) -> Expr 'PGBoolean
isNull (Expr a) =
  case Op.isNull (Op.Column a) of
    Op.Column b -> Expr b

-- | The PostgreSQL @NOT@ operator
not :: Expr 'PGBoolean -> Expr 'PGBoolean
not (Expr a) =
  case Op.not (Op.Column a) of
    Op.Column b -> Expr b

infix 4 ==.
infix 4 /=.
infixr 2 ||.
infix 4 <.
infix 4 <=.
infix 4 >.
infix 4 >=.

--------------------------------------------------------------------------------

class Insertable row where
  insertTable :: f row -> Op.Table row ()

instance (KnownSymbol (TableName rel),Generic (rel Insertion),GWriter (Rep (rel ExtractSchema)) (Rep (rel Insertion))) => Insertable (rel Insertion) where
  insertTable _ =
    Op.Table (symbolVal (Proxy :: Proxy (TableName rel)))
             (lmap from
                   (Op.TableProperties (gwriter (Proxy :: Proxy (Rep (rel ExtractSchema))))
                                       (Op.View ())))

class GWriter f g where
  gwriter :: proxy f -> Op.Writer (g x) ()

instance GWriter f f' => GWriter (M1 i c f) (M1 i c f') where
  gwriter _ =
    lmap (\(M1 x) -> x)
         (gwriter (Proxy :: Proxy f))

instance (GWriter f f',GWriter g g') => GWriter (f :*: g) (f' :*: g') where
  gwriter _ =
    dimap (\(l' :*: r') -> (l',r'))
          fst
          (gwriter (Proxy :: Proxy f) ***! gwriter (Proxy :: Proxy g))

instance KnownSymbol columnName => GWriter (K1 i (Proxy columnName)) (K1 i (Default (Expr t))) where
  gwriter _ =
    dimap (\(K1 def) ->
             case def of
               InsertDefault -> Op.Column (Op.DefaultInsertExpr)
               ProvideValue (Expr a) -> Op.Column a)
          (const ())
          (Op.required (symbolVal (Proxy :: Proxy columnName)))

instance KnownSymbol columnName => GWriter (K1 i (Proxy columnName)) (K1 i (Expr t)) where
  gwriter _ =
    dimap (\(K1 (Expr e)) -> Op.Column e)
          (const ())
          (Op.required (symbolVal (Proxy :: Proxy columnName)))

-- | Given a 'Table' and a collection of rows for that table, @INSERT@ this data
-- into PostgreSQL. The rows are specified as PostgreSQL expressions.
insert
  :: forall (rel :: (k -> *) -> *).
     Insertable (rel Insertion)
  => PG.Connection -> [rel Insertion] -> IO Int64
insert conn rows =
  Op.runInsertMany conn
                   (insertTable rows)
                   rows

-- | Insert a single row and return it. This is useful if you have columns that
-- have default values.
insert1Returning
  :: forall rel.
     (Insertable (rel Insertion),Selectable (rel Expr) (rel Interpret),ColumnView (Rep (rel ExtractSchema)) (Rep (rel Expr)),Generic (rel Expr))
  => PG.Connection -> rel Insertion -> IO (rel Interpret)
insert1Returning conn row =
  fmap head
       (Op.runInsertReturningExplicit
          (queryRunner :: Op.QueryRunner (rel Expr) (rel Interpret))
          conn
          (case insertTable [row] of
             Op.Table tableName props -> Op.Table tableName (remapProps props)
             Op.TableWithSchema a b props ->
               Op.TableWithSchema a
                                  b
                                  (remapProps props))
          row
          to)
  where remapProps (Op.TableProperties (Op.Writer f) _) =
          Op.TableProperties (Op.Writer f)
                             (Op.View (columnViewRep (Proxy :: Proxy (Rep (rel ExtractSchema)))))

-- | Given a 'Op.Query', filter the rows of the result set according to a
-- predicate.
filterQuery
  :: (a -> Expr 'PGBoolean) -> Op.Query a -> Op.Query a
filterQuery f t =
  fmap snd (first restrict . fmap (f &&& id) t)

--------------------------------------------------------------------------------
newtype PGOrdering a =
  PGOrdering (a -> [(Op.OrderOp,Op.PrimExpr)])
  deriving (Monoid)

asc :: (a -> Expr (b :: PGType)) -> PGOrdering a
asc f =
  PGOrdering
    (\x ->
       case f x of
         Expr a -> [(Op.OrderOp Op.OpAsc Op.NullsFirst,a)])

desc :: (a -> Expr (b :: PGType)) -> PGOrdering a
desc f =
  PGOrdering
    (\x ->
       case f x of
         Expr a -> [(Op.OrderOp Op.OpDesc Op.NullsFirst,a)])

data OrderNulls
  = NullsFirst
  | NullsLast
  deriving (Enum,Ord,Eq,Read,Show,Bounded)

orderNulls :: forall a b.
              (forall (x :: PGType). (a -> Expr x) -> PGOrdering a)
           -> OrderNulls
           -> (a -> Expr ('Nullable (b :: PGType)))
           -> PGOrdering a
orderNulls direction nulls f =
  case direction (\a ->
                    case f a of
                      Expr x -> Expr x :: Expr b) of
    PGOrdering g ->
      PGOrdering
        (\a ->
           map (first (\(Op.OrderOp orderOp _) -> Op.OrderOp orderOp nullsDir))
               (g a))
  where nullsDir =
          case nulls of
            NullsFirst -> Op.NullsFirst
            NullsLast -> Op.NullsLast

orderBy :: PGOrdering a -> Op.Query a -> Op.Query a
orderBy (PGOrdering f) = Op.orderBy (Op.Order f)

{- $intro

Welcome to @opaleye-tf@, a library to query and interact with PostgreSQL
databases. As the name suggests, this library builds on top of the terrific
@opaleye@ library, but provides a different API that the author believes
provides more succinct code with better type inference.

The basic idea behind @opaleye-tf@ is to \"pivot\" around the ideas in
@opaleye@. The current idiomatic usage of Opaleye is to define your records as
data types where each field is parameterized. Opaleye then varies all of these
parameters together. @opaleye-tf@ makes the observation that if all of these
vary uniformly, then there should only be /one/ parameter, and thus we have
records that are parameterized by functors.

To take an example, let's consider a simple schema for Hackage - the repository
of Haskell libraries.

@
data Package f =
  Package { packageName :: 'Col' f 'PGText'
          , packageAuthor :: Col f \''PGInteger'
          , packageMaintainerId :: Col f (\''PGNull' \''PGInteger')
          }

data User f =
  User { userId :: 'Col' f \''PGInteger'
       , userName :: 'Col' f \''PGText'
       , userBio :: 'Col' f (\''PGNull' 'PGText')
       }
@

In this example, each record (@Package@ and @User@) correspond to tables in a
PostgreSQL database. These records are parameterized over functors @f@, which
will provide /meaning/ depending on how the record is used. Notice all that
we specify the types as they are in PostgreSQL. @opaleye-tf@ primarily focuses
on the flow of information /from/ the database.

One type of meaning that we can give to tables is to map their fields to their
respective columns. This is done by choosing 'Table' as our choice of @f@:

@
packageTable :: Package ('Table' "package")
packageTable = Package { packageName = "name"
                       , packageAuthor = "author_id"
                       , packageMaintainerId = "maintainer_id" }

userTable :: User ('Table' "user")
userTable = User { userId = "id"
                 , userName = "name"
                 , userBio = "bio"
                 }
@

Now that we have full definitions of our tables, we can perform some @SELECT@s.
First, let's list all known packages:

@
listAllPackages :: 'PG.Connection' -> IO [Package 'Interpret']
listAllPackages c = 'select' ('queryTable' packageTable)
@

This computation now returns us a list of @Package@s where @f@ has been set as
'Interpret'. The 'Interpret' functor is responsible for defining the mapping
from PostgreSQL types (such as 'PGText') to Haskell types (such as 'Text').

Another choice of @f@ occurs when we perform a left join. For example, here
is the /query/ to list all packages with their (optional) maintainers:

@
listAllPackagesAndMaintainersQuery :: 'Query' (Package 'Expr', User ('Compose' 'Expr' 'PGNull'))
listAllPackagesAndMaintainersQuery =
  'leftJoin' (\p m -> packageMaintainerId p '==.' userId m)
           ('queryTable' package)
           ('queryTable' user))
@

This query communicates that we will have a collection of 'Expr'essions that
correspond to columns in the @Package@ table, and also a collection of
'Expr'essions that are columns in the @User@ table. However, as the user table
is only present as a left join, all of these columns might be @NULL@ - indicated
by the composition of 'PGNull' with 'Expr'.

@opaleye-tf@ is smart enough to collapse data together where convenient.
For example, if we look at just the final types of @userId@ and @userBio@ in the
context of the left join:

@
> :t fmap (\\(_, u) -> userId u) listAllPackagesAndMaintainersQuery
Query (Expr (PGNull PGInteger))

> :t fmap (\\(_, u) -> userBio u) listAllPackagesAndMaintainersQuery
Query (Expr (PGNull PGText))
@

Which are as expected.

Finally, when executing queries that contain left joins, @opaleye-tf@ is able to
invert the possible @NULLs@ over the whole record:

@
> :t \conn -> select conn listAllPackagesAndMaintainersQuery
Connection -> IO [(Package Interpret, Maybe (User Interpret))]
@

Notice that @User (Compose Expr PGNull)@ was mapped to @Maybe (User Interpret)@.

-}
