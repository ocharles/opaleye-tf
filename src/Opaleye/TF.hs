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
{-# LANGUAGE ViewPatterns #-}

module Opaleye.TF
       ( -- $intro
         -- * The 'Col' type family
         type Col,

         -- * Mapping PostgreSQL types
         PGType(..), Lit(..), null, toNullable, pgNow, mapExpr,

         -- * Defining tables
         ExtractSchema, TableName, Column(..), PGNull(..), PGDefault(..),

         -- * Querying tables
         queryTable, queryTableBy, queryTableOn, Expr, select, leftJoin, restrict, (||.), (&&.), ilike, elem_, isNull, not, (++.),
         filterQuery, asc, desc, orderNulls, OrderNulls(..), orderBy, limit, offset,
         leftJoinTableOn, leftJoinOn,
         PGEq(..), PGOrd(..), (/=.), (?=), SingleColumn(..), mkSingle, in_,

         -- ** Aggregation
         Aggregate(..), aggregate, count, countDistinct, groupBy, PGMax(max), PGMin(min), mapAggregate,

         -- * Inserting data
         insert, insert1Returning, Insertion, Default(..), overrideDefault, insertDefault,

         -- * Updating data
         update,

         -- * Deleting data
         delete,

         -- * TODO Organize
         Query, unsafeFromNullable,

         -- * Implementation details
         Compose(..), Interpret, Selectable, Insertable, ColumnView, queryRunner, Aggregates(..)

         )
       where

import Data.Semigroup (Semigroup)
import Control.Applicative
import Control.Arrow (first, (&&&))
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
import qualified Opaleye.Aggregate as Op
import qualified Opaleye.Column as Op
import qualified Opaleye.Internal.Aggregate as Op
import qualified Opaleye.Internal.Column as Op
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op
import qualified Opaleye.Internal.Join as Op
import qualified Opaleye.Internal.Order as Op
import qualified Opaleye.Internal.PackMap as Op
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.QueryArr as Op
import qualified Opaleye.Internal.RunQuery as Op
import qualified Opaleye.Internal.Table as Op
import qualified Opaleye.Internal.TableMaker as Op
import qualified Opaleye.Internal.Unpackspec as Op
import qualified Opaleye.Join as Op
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
import Opaleye.TF.Scope (Scope(S,Z))
import Opaleye.TF.Table
import qualified Opaleye.Table as Op hiding (required)
import Prelude hiding (null, (.), id, not, max, min)

--------------------------------------------------------------------------------
newtype Query (s :: Scope) a = Query (Op.Query a)

instance Functor (Query s) where
  fmap f (Query q) = Query (fmap f q)

instance Applicative (Query s) where
  pure a = Query (pure a)
  Query a <*> Query b = Query (a <*> b)

instance Monad (Query s) where
  return = pure
  Query (Op.QueryArr x) >>= f =
    Query (Op.QueryArr
             (\((),pq,t) ->
                case x ((),pq,t) of
                  (x',pq',t') ->
                    case f x' of
                      Query (Op.QueryArr y) -> y ((),pq',t')))

--------------------------------------------------------------------------------
leftJoinTableOn
  :: (Generic (rel ExtractSchema),Generic (rel (Expr s)),InjPackMap (Rep (rel (Expr s))),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr s))),KnownSymbol (TableName rel),Generic (rel (Compose (Expr s) 'Nullable)),GToNull (Rep (rel (Expr s))) (Rep (rel (Compose (Expr s) 'Nullable))),NullableBoolean boolean)
  => (rel (Expr s) -> Expr s boolean)
  -> Query s (rel (Compose (Expr s) 'Nullable))
leftJoinTableOn predicate = leftJoinOn predicate queryTable

-- TODO Is it really sound to use the same scope here? Perhaps requires
-- LATERAL joins.
leftJoinOn
  :: (ToNull a maybeA, NullableBoolean boolean)
  => (a -> Expr s boolean) -> Query s a -> Query s maybeA
leftJoinOn predicate q =
  Query $
  Op.QueryArr $
  \((),left,t) ->
    case q of
      Query (Op.QueryArr f) ->
        case f ((),PQ.Unit,t) of
          (rightRel,pqR,t') ->
            (toNull rightRel
            ,PQ.Join PQ.LeftJoin
                     (case toNullableBoolean (predicate rightRel) of
                        Expr a -> a)
                     left
                     pqR
            ,t')

-- | Shorthand to query a table using a single (field) accessor
queryTableBy :: (Generic (rel (Expr s)),Generic (rel ExtractSchema),InjPackMap (Rep (rel (Expr s))),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr s))),KnownSymbol (TableName rel),PGEq prim)
             => (rel (Expr s) -> Expr s prim)
             -> Expr s prim
             -> Query s (rel (Expr s))
queryTableBy accessor r = queryTableOn ((==. r) . accessor)

-- | Shorthand to filter
queryTableOn :: (Generic (rel (Expr s)),Generic (rel ExtractSchema),InjPackMap (Rep (rel (Expr s))),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr s))),KnownSymbol (TableName rel), NullableBoolean boolean)
             => (rel (Expr s) -> Expr s boolean)
             -> Query s (rel (Expr s))
queryTableOn predicate = filterQuery predicate queryTable

--------------------------------------------------------------------------------

-- | 'queryTable' moves from 'Table' to 'Expr'. Accessing the fields of your
-- table record will now give you expressions to view data in the individual
-- columns.
queryTable :: forall (rel :: (k -> *) -> *) (s :: Scope).
              (Generic (rel ExtractSchema),Generic (rel (Expr s)),InjPackMap (Rep (rel (Expr s))),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr s))),KnownSymbol (TableName rel))
           => Query s (rel (Expr s))
queryTable =
  Query $
  Op.queryTableExplicit
    columnMaker
    (Op.Table (symbolVal (Proxy :: Proxy (TableName rel)))
              (Op.TableProperties undefined
                                  (Op.View columnView)))
  where columnView = columnViewRep (Proxy :: Proxy (Rep (rel ExtractSchema)))
        columnMaker =
          Op.ColumnMaker
            (Op.PackMap (\inj columns -> fmap to (injPackMap inj columns)))

-- | A type class to get the column names out of the record.
class ColumnView f g where
  columnViewRep :: proxy f -> g x

instance ColumnView f f' => ColumnView (M1 i c f) (M1 i c f') where
  columnViewRep _ = M1 (columnViewRep (Proxy :: Proxy f))

instance (ColumnView f f', ColumnView g g') => ColumnView (f :*: g) (f' :*: g') where
  columnViewRep _ = columnViewRep (Proxy :: Proxy f) :*: columnViewRep (Proxy :: Proxy g)

instance KnownSymbol columnName => ColumnView (K1 i (Proxy columnName)) (K1 i (Expr s colType)) where
  columnViewRep _ = K1 (Expr (Op.BaseTableAttrExpr (symbolVal (Proxy :: Proxy columnName))))

-- A type to generate that weird PackMap thing queryTableExplicit wants.
-- Basically associating column symbol names with the given record.
class InjPackMap g where
  injPackMap :: Applicative f => (Op.PrimExpr -> f Op.PrimExpr) -> g x -> f (g x)

instance InjPackMap f => InjPackMap (M1 i c f) where
  injPackMap f (M1 a) = fmap M1 (injPackMap f a)

instance (InjPackMap f, InjPackMap g) => InjPackMap (f :*: g) where
  injPackMap f (a :*: b) = liftA2 (:*:) (injPackMap f a) (injPackMap f b)

instance InjPackMap (K1 i (Expr s colType)) where
  injPackMap f (K1 (Expr prim)) = fmap (K1 . Expr) (f prim)

--------------------------------------------------------------------------------

-- | 'select' executes a PostgreSQL query as a @SELECT@ statement, returning
-- data mapped to Haskell values.
select
  :: Selectable pg haskell
  => PG.Connection -> Query 'Z pg -> IO [haskell]
select conn (Query q) = Op.runQueryExplicit queryRunner conn q

-- A type class for selectable things, so we can return a table or a tuple.
class Selectable expr haskell | expr -> haskell where
  queryRunner :: Op.QueryRunner expr haskell

instance (haskell ~ Col Interpret t, PG.FromField haskell) => Selectable (Expr s t) haskell where
  queryRunner = lmap (\(Expr a) -> Op.Column a) (Op.queryRunner Op.fieldQueryRunnerColumn)

-- A scary instance for interpreting Expr to Haskell types generically.
instance (Generic (rel (Expr s)),ParseRelRep (Rep (rel (Expr s))) (Rep (rel Interpret)),Generic (rel Interpret),UnpackspecRel (Rep (rel (Expr s))), HasFields (Rep (rel (Expr s)))) =>
           Selectable (rel (Expr s)) (rel Interpret) where
  queryRunner = gqueryRunner

-- The same instance but for left joins.
instance (Generic (rel (Compose (Expr s) 'Nullable)),ParseRelRep (Rep (rel (Compose (Expr s) 'Nullable))) (Rep (rel (Compose Interpret 'Nullable))),Generic (rel (Compose Interpret 'Nullable)),UnpackspecRel (Rep (rel (Compose (Expr s) 'Nullable))), Generic (rel Interpret), DistributeMaybe (rel (Compose Interpret 'Nullable)) (rel Interpret), HasFields (Rep (rel (Compose (Expr s) 'Nullable)))) =>
           Selectable (rel (Compose (Expr s) 'Nullable)) (Maybe (rel Interpret)) where
  queryRunner = fmap distributeMaybe (gqueryRunner :: Op.QueryRunner (rel (Compose (Expr s) 'Nullable)) (rel (Compose Interpret 'Nullable)))

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

instance (haskell ~ Col Interpret colType, PG.FromField haskell) => ParseRelRep (K1 i (Expr s colType)) (K1 i haskell) where
  parseRelRep (K1 _) = fmap K1 PG.field

class UnpackspecRel rep where
  unpackRel :: Applicative f => (Op.PrimExpr -> f Op.PrimExpr) -> rep x -> f ()

instance UnpackspecRel f => UnpackspecRel (M1 i c f) where
  unpackRel inj (M1 a) = unpackRel inj a

instance (UnpackspecRel a, UnpackspecRel b) => UnpackspecRel (a :*: b) where
  unpackRel inj (a :*: b) = unpackRel inj a *> unpackRel inj b

instance UnpackspecRel (K1 i (Expr s colType)) where
  unpackRel inj (K1 (Expr prim)) = void (inj prim)

--------------------------------------------------------------------------------

-- | A left join takes two queries and performs a relational @LEFT JOIN@ between
-- them. The left join has all rows of the left query, joined against zero or
-- more rows in the right query. If the join matches no rows in the right table,
-- then all columns will be @null@.
leftJoin :: (ToNull right nullRight, NullableBoolean boolean)
         => (left -> right -> Expr s boolean)
         -> Query s left
         -> Query s right
         -> Query s (left,nullRight)
leftJoin f (Query l) (Query r) =
  Query $
  Op.leftJoinExplicit
    undefined
    undefined
    (Op.NullMaker toNull)
    l
    r
    (\(l',r') ->
       case f l' r' of
         Expr prim -> Op.Column prim)

class ToNull expr exprNull | expr -> exprNull where
  toNull :: expr -> exprNull

instance (GToNull (Rep (rel (Expr s))) (Rep (rel (Compose (Expr s) 'Nullable))), Generic (rel (Expr s)), Generic (rel (Compose (Expr s) 'Nullable))) => ToNull (rel (Expr s)) (rel (Compose (Expr s) 'Nullable)) where
  toNull = to . gtoNull . from

class GToNull f g | f -> g where
  gtoNull :: f x -> g x

instance GToNull f f' => GToNull (M1 i c f) (M1 i c f') where
  gtoNull (M1 f) = M1 (gtoNull f)

instance (GToNull f f', GToNull g g') => GToNull (f :*: g) (f' :*: g') where
  gtoNull (f :*: g) = gtoNull f :*: gtoNull g

instance (t' ~ Col (Compose (Expr s) 'Nullable) t, t' ~ (Expr s) x) => GToNull (K1 i (Expr s t)) (K1 i t') where
  gtoNull (K1 (Expr prim)) = K1 (Expr prim)

--------------------------------------------------------------------------------

-- | Apply a @WHERE@ restriction to a table. Like 'guard' for 'MonadPlus'.
restrict :: NullableBoolean boolean => Expr s boolean -> Query s ()
restrict (toNullableBoolean -> Expr b) =
  Query (Op.keepWhen (const (Op.Column b)))

-- | Boolean operations, overloaded to also work on nullable values.
class PGBoolean a where
  true, false :: Expr s a
  not :: Expr s a -> Expr s a
  (||.), (&&.) :: Expr s a -> Expr s a -> Expr s a

instance PGBoolean 'PGBoolean where
  Expr a ||. Expr b =
    case Op.Column a Op..|| Op.Column b of
      Op.Column c -> Expr c

  Expr a &&. Expr b =
    case Op.Column a Op..&& Op.Column b of
      Op.Column c -> Expr c

  not (Expr a) =
    case Op.not (Op.Column a) of
      Op.Column b -> Expr b

  true = lit True
  false = lit False

-- | All operations use the same semantics as those in PostgreSQL. This does
-- *not* correspond to a true Boolean algebra, as @true ||. null@ is @null@,
-- rather than @true@.
instance PGBoolean ('Nullable 'PGBoolean) where
  Expr a ||. Expr b =
    case Op.Column a Op..|| Op.Column b of
      Op.Column c -> Expr c

  Expr a &&. Expr b =
    case Op.Column a Op..&& Op.Column b of
      Op.Column c -> Expr c

  not (Expr a) =
    case Op.not (Op.Column a) of
      Op.Column b -> Expr b

  true = lit (Just True)
  false = lit (Just False)

-- | A helper similar to the PostgreSQL @IN@ operator for matching a list of expressions.
--   This may be replaced in the future by
-- @
--     in_ :: Expr s a -> Expr s ('PGArray a) -> Expr s 'PGBoolean
-- @
elem_ :: PGEq a => Expr s a -> [Expr s a] -> Expr s 'PGBoolean
elem_ x = Prelude.foldl (\b y -> x ==. y ||. b) (lit False)

-- | The PostgreSQL @ILIKE@ operator.
ilike :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGBoolean
Expr a `ilike` Expr b =
  case Op.binOp (Op.OpOther "ILIKE") (Op.Column a) (Op.Column b) of
    Op.Column c -> Expr c

-- | The PostgreSQL @IS NULL@ operator
isNull :: Expr s ('Nullable a) -> Expr s 'PGBoolean
isNull (Expr a) =
  case Op.isNull (Op.Column a) of
    Op.Column b -> Expr b

-- | The PostgreSQL string concatenation operator.
(++.) :: Expr s 'PGText -> Expr s 'PGText -> Expr s 'PGText
Expr a ++. Expr b =
  case Op.Column a Op..++ Op.Column b of
    Op.Column c -> Expr c

infix 4 ==.
infix 4 /=.
infixr 2 ||.
infixr 2 &&.
infix 4 <.
infix 4 <=.
infix 4 >.
infix 4 >=.

--------------------------------------------------------------------------------

class Insertable row where
  insertTable :: f row -> Op.Table row ()

instance (KnownSymbol (TableName rel),Generic (rel f),GWriter (Rep (rel ExtractSchema)) (Rep (rel f))) => Insertable (rel f) where
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

instance KnownSymbol columnName => GWriter (K1 i (Proxy columnName)) (K1 i (Default (Expr s t))) where
  gwriter _ =
    dimap (\(K1 def) ->
             case def of
               InsertDefault -> Op.Column (Op.DefaultInsertExpr)
               ProvideValue (Expr a) -> Op.Column a)
          (const ())
          (Op.required (symbolVal (Proxy :: Proxy columnName)))

instance KnownSymbol columnName => GWriter (K1 i (Proxy columnName)) (K1 i (Expr s t)) where
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

update :: forall boolean s rel.
          (Insertable (rel (Expr s)),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr s))),Generic (rel (Expr s)), NullableBoolean boolean)
       => PG.Connection
       -> (rel (Expr s) -> Expr s boolean)
       -> (rel (Expr s) -> rel (Expr s))
       -> IO Int64
update conn where_ up =
  Op.runUpdate
    conn
    (case insertTable (Proxy :: Proxy (rel (Expr s))) of
       Op.Table t props -> Op.Table t (remapProps props)
       Op.TableWithSchema a b props ->
         Op.TableWithSchema a
                            b
                            (remapProps props))
    (up . to)
    (\rel ->
       case where_ (to rel) of
         Expr a -> Op.Column a)
  where remapProps (Op.TableProperties (Op.Writer f) _) =
          Op.TableProperties (Op.Writer f)
                             (Op.View (columnViewRep (Proxy :: Proxy (Rep (rel ExtractSchema)))))

-- | Insert a single row and return it. This is useful if you have columns that
-- have default values.
insert1Returning
  :: forall rel.
     (Insertable (rel Insertion),Selectable (rel (Expr 'Z)) (rel Interpret),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr 'Z))),Generic (rel (Expr 'Z)))
  => PG.Connection -> rel Insertion -> IO (rel Interpret)
insert1Returning conn row =
  fmap head
       (Op.runInsertReturningExplicit
          (queryRunner :: Op.QueryRunner (rel (Expr 'Z)) (rel Interpret))
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

-- | Given a 'Table' and a predicate, @DELETE@ all rows that match.
delete
  :: forall (rel :: (k -> *) -> *) boolean.
     (Generic (rel (Expr 'Z)), ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr 'Z))),KnownSymbol (TableName rel))
  => PG.Connection
  -> (rel (Expr 'Z) -> Expr 'Z boolean)
  -> IO Int64
delete conn where_ =
  Op.runDelete conn
               (Op.Table (symbolVal (Proxy :: Proxy (TableName rel)))
                         (Op.TableProperties undefined
                                             (Op.View (columnViewRep (Proxy :: Proxy (Rep (rel ExtractSchema)))))))
               (\rel ->
                 case where_ (to rel) of
                   Expr a -> Op.Column a)

-- | Given a 'Op.Query', filter the rows of the result set according to a
-- predicate.
filterQuery
  :: (NullableBoolean boolean)
  => (a -> Expr s boolean) -> Query s a -> Query s a
filterQuery f (Query t) =
  Query (fmap snd (first restrict' . fmap (f &&& id) t))
  where restrict' =
          lmap ((\(Expr prim) -> Op.Column prim) . toNullableBoolean) Op.restrict

--------------------------------------------------------------------------------
newtype PGOrdering a =
  PGOrdering (a -> [(Op.OrderOp,Op.PrimExpr)])
  deriving (Semigroup, Monoid)

asc :: PGOrd b => (a -> Expr s b) -> PGOrdering a
asc f =
  PGOrdering
    (\x ->
       case f x of
         Expr a -> [(Op.OrderOp Op.OpAsc Op.NullsFirst,a)])

desc :: PGOrd b => (a -> Expr s b) -> PGOrdering a
desc f =
  PGOrdering
    (\x ->
       case f x of
         Expr a -> [(Op.OrderOp Op.OpDesc Op.NullsFirst,a)])

data OrderNulls
  = NullsFirst
  | NullsLast
  deriving (Enum,Ord,Eq,Read,Show,Bounded)

orderNulls :: forall a b s.
              PGOrd b
           => ((a -> Expr s b) -> PGOrdering a)
           -> OrderNulls
           -> (a -> Expr s ('Nullable b))
           -> PGOrdering a
orderNulls direction nulls f =
  case direction (\a ->
                    case f a of
                      Expr x -> Expr x :: Expr s b) of
    PGOrdering g ->
      PGOrdering
        (\a ->
           map (first (\(Op.OrderOp orderOp _) -> Op.OrderOp orderOp nullsDir))
               (g a))
  where nullsDir =
          case nulls of
            NullsFirst -> Op.NullsFirst
            NullsLast -> Op.NullsLast

orderBy
  :: PGOrdering a -> Query s a -> Query s a
orderBy (PGOrdering f) (Query q) = Query (Op.orderBy (Op.Order f) q)

limit
  :: Int -> Query s a -> Query s a
limit n (Query q) = Query (Op.limit n q)

offset
  :: Int -> Query s a -> Query s a
offset n (Query q) = Query (Op.offset n q)

--------------------------------------------------------------------------------
data Aggregate (s :: Scope) (t :: k) =
  Aggregate (Maybe Op.AggrOp)
            (Expr s t)
            Op.AggrDistinct

type instance Col (Aggregate s) (t :: k) = Aggregate s t

class Aggregates s a b | b s -> a where
  compileAggregator :: Proxy s -> Op.Aggregator a b

instance (Generic (rel (Aggregate ('S s))), Generic (rel (Expr s))
         ,GAggregator (Rep (rel (Aggregate ('S s))))
                      (Rep (rel (Expr s)))) => Aggregates s (rel (Aggregate ('S s))) (rel (Expr s)) where
  compileAggregator _ = dimap from to gaggregator

newtype SingleColumn a f = SingleColumn { unColumn :: Col f a }
  deriving (Generic)

mkSingle :: (Col f a ~ g a) => g a -> SingleColumn a f
mkSingle = SingleColumn

class GAggregator f g where
  gaggregator :: Op.Aggregator (f a) (g b)

instance GAggregator f g => GAggregator (M1 i c f) (M1 i c g) where
  gaggregator = dimap (\(M1 a) -> a) M1 gaggregator

instance (GAggregator a c,GAggregator b d) => GAggregator (a :*: b) (c :*: d) where
  gaggregator =
    dimap (\(l :*: r) -> (l,r))
          (\(l,r) -> l :*: r)
          (gaggregator ***! gaggregator)

instance (Expr s b ~ Col (Expr s) a) => GAggregator (K1 i (Aggregate ('S s) a)) (K1 i (Expr s b)) where
  gaggregator =
    Op.Aggregator
      (Op.PackMap
         (\f (K1 (Aggregate op (Expr e) distinct)) ->
            fmap (K1 . Expr)
                 (f (liftA3 (,,)
                            op
                            (pure [])
                            (pure distinct)
                    ,e))))

aggregate :: forall row s.
             Aggregates s (row (Aggregate ('S s))) (row (Expr s))
          => Query ('S s) (row (Aggregate ('S s))) -> Query s (row (Expr s))
aggregate (Query q) =
  Query (Op.aggregate (compileAggregator (Proxy :: Proxy s))
                      q)

count :: Expr s a -> Aggregate s 'PGBigint
count (Expr a) =
  Aggregate (Just Op.AggrCount)
            (Expr a)
            Op.AggrAll

countDistinct :: Expr s a -> Aggregate s 'PGBigint
countDistinct (Expr a) =
  Aggregate (Just Op.AggrCount)
            (Expr a)
            Op.AggrDistinct

-- | The class of data types that can be aggregated under the @max@ operation
class PGMax (a :: k)  where
  max :: Expr s a -> Aggregate s a
  max (Expr a) =
    Aggregate (Just Op.AggrMax)
              (Expr a)
              Op.AggrAll

instance PGMax 'PGBigint
instance PGMax ('PGCharacter n)
instance PGMax ('PGVarchar n)
instance PGMax 'PGDate
instance PGMax 'PGDouble
instance PGMax 'PGInet
instance PGMax 'PGInteger
instance PGMax 'PGInterval
instance PGMax 'PGMoney
instance PGMax ('PGNumeric p s)
instance PGMax 'PGReal
instance PGMax 'PGSmallint
instance PGMax 'PGText
instance PGMax ('PGTime tz)
instance PGMax ('PGTimestamp tz)

instance PGMax a => PGMax ('Nullable a) where
  max (Expr a) =
    mapAggregate toNullable'
                 (max (Expr a :: Expr s a))
    where toNullable' = Cast

-- | The class of data types that can be aggregated under the @min@ operation
class PGMin (a :: k)  where
  min :: Expr s a -> Aggregate s a
  min (Expr a) =
    Aggregate (Just Op.AggrMin)
              (Expr a)
              Op.AggrAll

instance PGMin 'PGBigint
instance PGMin ('PGCharacter n)
instance PGMin ('PGVarchar n)
instance PGMin 'PGDate
instance PGMin 'PGDouble
instance PGMin 'PGInet
instance PGMin 'PGInteger
instance PGMin 'PGInterval
instance PGMin 'PGMoney
instance PGMin ('PGNumeric p s)
instance PGMin 'PGReal
instance PGMin 'PGSmallint
instance PGMin 'PGText
instance PGMin ('PGTime tz)
instance PGMin ('PGTimestamp tz)

instance PGMin a => PGMin ('Nullable a) where
  min (Expr a) =
    mapAggregate toNullable'
                 (min (Expr a :: Expr s a))
    where toNullable' = Cast

groupBy :: PGEq a => Expr s a -> Aggregate s a
groupBy (Expr a) = Aggregate Nothing (Expr a) Op.AggrAll

--------------------------------------------------------------------------------
class PGEq (a :: k) where
  -- | The PostgreSQL @=@ operator.
  (==.) :: Expr s a -> Expr s a -> Expr s 'PGBoolean

  -- XXX It would be really nice if this could automatically be inferred.
  -- | Is the underlying equality function @STRICT@ (PostgreSQL terminology)?
  -- Strict equality means that if either of the inputs are null, then the
  -- result of @==.@ is @null@. This is the case for primitive equality, and
  -- the default implementation is to return 'True'.
  strictEquality :: Expr s ('Nullable a) -> Bool
  strictEquality _ = True

-- | The PostgreSQL @!=@ or @<>@ operator.
(/=.) :: PGEq a => Expr s a -> Expr s a -> Expr s 'PGBoolean
a /=. b = not (a ==. b)

instance PGEq (a :: PGType) where
  Expr a ==. Expr b =
    case Op.Column a Op..== Op.Column b of
      Op.Column c -> Expr c

class NullableBoolean (a :: k) where
  toNullableBoolean :: Expr s a -> Expr s ('Nullable 'PGBoolean)

instance NullableBoolean 'PGBoolean where
  toNullableBoolean = toNullable

instance NullableBoolean ('Nullable 'PGBoolean) where
  toNullableBoolean = id

(?=) :: PGEq a
     => Expr s ('Nullable a)
     -> Expr s ('Nullable a)
     -> Expr s ('Nullable 'PGBoolean)
a ?= b
  | strictEquality a =
    toNullable (mapExpr unsafeFromNullable a ==. mapExpr unsafeFromNullable b)
  | otherwise =
    nullable (lit Nothing)
             (\a' ->
                nullable (lit Nothing)
                         (\b' -> toNullable (a' ==. b'))
                         b)
             a

unsafeFromNullable :: 'Nullable a ~> a
unsafeFromNullable = Cast

--------------------------------------------------------------------------------
class PGEq a => PGOrd (a :: k) where
  -- | The PostgreSQL @<@ operator.
  (<.) :: Expr s a -> Expr s a -> Expr s 'PGBoolean
  a <. b = not (a >=. b)

  -- | The PostgreSQL @<=@ operator.
  (<=.) :: Expr s a -> Expr s a -> Expr s 'PGBoolean
  a <=. b = not (a >. b)

  -- | The PostgreSQL @>@ operator.
  (>.) :: Expr s a -> Expr s a -> Expr s 'PGBoolean
  a >. b = not (a <=. b)

  -- | The PostgreSQL @>@ operator.
  (>=.) :: Expr s a -> Expr s a -> Expr s 'PGBoolean
  a >=. b = not (a <. b)

instance PGOrd (a :: PGType) where
  Expr a <. Expr b =
    case Op.binOp Op.OpLt (Op.Column a) (Op.Column b) of
      Op.Column c -> Expr c

  Expr a <=. Expr b =
    case Op.binOp Op.OpLtEq (Op.Column a) (Op.Column b) of
      Op.Column c -> Expr c

  Expr a >. Expr b =
    case Op.binOp Op.OpGt (Op.Column a) (Op.Column b) of
      Op.Column c -> Expr c

  Expr a >=. Expr b =
    case Op.binOp Op.OpGtEq (Op.Column a) (Op.Column b) of
      Op.Column c -> Expr c

--------------------------------------------------------------------------------
mapAggregate :: a ~> b -> Aggregate s a -> Aggregate s b
mapAggregate Cast (Aggregate op (Expr e) d) = Aggregate op (Expr e) d
mapAggregate Id (Aggregate op (Expr e) d) = Aggregate op (Expr e) d

in_
  :: PGEq a
  => Expr s a -> [Expr s a] -> Expr s 'PGBoolean
in_ x = Prelude.foldl (\b y -> x ==. y ||. b) (lit False)

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
