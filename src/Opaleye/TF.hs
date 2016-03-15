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

module Opaleye.TF
       ( -- $intro
         -- * The 'Col' type family
         type Col,

         -- * Mapping PostgreSQL types
         PGTypes(..), lit,

         -- * Defining tables
         Table(..),

         -- * Querying tables
         queryTable, Expr, select, leftJoin, restrict, (==.),

         -- * Inserting data
         insert, Insertion, Default(..),

         -- * Implementation details
         Compose(..), InterpretPGType, InterpretNull, Interpret)
       where

import Control.Applicative
import Control.Arrow
import Control.Monad (void)
import Control.Monad.Trans.State.Strict
       (State, get, modify, state, execState, runState)
import Data.Int
import Data.Profunctor
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import GHC.Generics
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Opaleye.Column as Op
import qualified Opaleye.Internal.Aggregate as Op
import qualified Opaleye.Internal.Column as Op
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Op
import qualified Opaleye.Internal.Join as Op
import qualified Opaleye.Internal.Optimize as Op
import qualified Opaleye.Internal.PackMap as Op
import qualified Opaleye.Internal.PrimQuery as Op hiding (restrict)
import qualified Opaleye.Internal.Print as Op
import qualified Opaleye.Internal.RunQuery as Op
import qualified Opaleye.Internal.Sql as Sql
import qualified Opaleye.Internal.Table as Op
import qualified Opaleye.Internal.TableMaker as Op
import qualified Opaleye.Internal.Tag as Op
import qualified Opaleye.Internal.Unpackspec as Op
import qualified Opaleye.Join as Op
import qualified Opaleye.Manipulation as Op
import qualified Opaleye.Operators as Op
import qualified Opaleye.PGTypes as Op
import qualified Opaleye.QueryArr as Op
import qualified Opaleye.RunQuery as Op
import qualified Opaleye.Sql as Op
import qualified Opaleye.Table as Op hiding (required)

-- | This type family is responsible for a lot of magic, and is indispensible.
-- Essentially, the 'Col' type family lets
-- It's basically doing function application at the type level, but with a bit
-- of normalization thrown in.

type family Col (f :: a -> b) (x :: a) :: b where
  -- If we use the Interpret functor, then we'll use defunctionalisation to
  -- get down to something in *
  Col Interpret x = Apply InterpretPGType x

  -- The Insertion functor is like Interpret, in that it maps to ordinary
  -- Haskell types. However, it also maps PGDefault to a special Maybe-like
  -- type to allow the use of 'DEFAULT'.
  Col Insertion (PGDefault t) = Default (Expr t)
  Col Insertion x = Expr x

  -- InterpretNull is used for left joins.
  -- If we use InterpretNull with PGNull, then we get a Maybe of whatever t
  -- is. This avoids a double Maybe.
  Col (Compose Interpret PGNull) (PGNull t) = Maybe (Apply InterpretPGType t)

  -- Otherwise, we still get a Maybe of whatever the underlying type is.
  Col (Compose Interpret PGNull) x = Maybe (Col Interpret x)

  -- In the process of a left join, we work under Compose Expr PGNull.
  -- This basically does the same as InterpretNull PGNull - collapsing
  -- multiple PGNull layers together.
  Col (Compose Expr PGNull) (PGNull t) = Col (Compose Expr PGNull) t

  -- If the given column isn't nullable, make it nullable.
  Col (Compose Expr PGNull) t = Col Expr (PGNull t)

 -- If I don't know what f is, then just give me f x.
  Col f x = f x

--------------------------------------------------------------------------------

-- Table definitions are mostly in the types.
newtype Table (tableName :: Symbol) (columnType :: k) = Column String

-- This handy instance gives us a bit more sugar when defining a table.
instance IsString (Table tableName columnType) where fromString = Column

--------------------------------------------------------------------------------

-- Just like Opaleye's column, but kind polymorphic.
data Expr (columnType :: k) = Expr Op.PrimExpr

-- queryTable moves from working under Table to working under Expr.
queryTable :: forall table tableName. (KnownSymbol tableName, TableLike table tableName)
           => table (Table tableName) -> Op.Query (table Expr)
queryTable t =
  Op.queryTableExplicit
    columnMaker
    (Op.Table (symbolVal (Proxy :: Proxy tableName))
              (Op.TableProperties undefined
                                  (Op.View columnView)))
  where columnView = columnViewRep (from t)
        columnMaker =
          Op.ColumnMaker
            (Op.PackMap
               (\inj columns ->
                  fmap to (injPackMap inj columns)))

-- This should probably be a type class so I don't force generics on people.
type TableLike t tableName = (Generic (t Expr),Generic (t (Table tableName)), InjPackMap (Rep (t Expr)), ColumnView (Rep (t (Table tableName))) (Rep (t Expr)))

-- A type class to get the column names out of the record.
class ColumnView f g where
  columnViewRep :: f x -> g x

instance ColumnView f f' => ColumnView (M1 i c f) (M1 i c f') where
  columnViewRep (M1 f) = M1 (columnViewRep f)

instance (ColumnView f f', ColumnView g g') => ColumnView (f :*: g) (f' :*: g') where
  columnViewRep (f :*: g) = columnViewRep f :*: columnViewRep g

instance ColumnView (K1 i (Table tableName colType)) (K1 i (Expr colType)) where
  columnViewRep (K1 (Column name)) = K1 (Expr (Op.BaseTableAttrExpr name))

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

-- select moves from Expr to Interpret (or InterpretNull).

select :: Selectable pg haskell => PG.Connection -> Op.Query pg -> IO [haskell]
select conn = Op.runQueryExplicit queryRunner conn

-- A type class for selectable things, so we can return a table or a tuple.
class Selectable expr haskell | expr -> haskell where
  queryRunner :: Op.QueryRunner expr haskell

-- A scary instance for interpreting Expr to Haskell types generically.
instance (Generic (rel Expr),ParseRelRep (Rep (rel Expr)) (Rep (rel Interpret)),Generic (rel Interpret),UnpackspecRel (Rep (rel Expr)), HasFields (Rep (rel Expr))) =>
           Selectable (rel Expr) (rel Interpret) where
  queryRunner = gqueryRunner

-- The same instance but for left joins.
instance (Generic (rel (Compose Expr PGNull)),ParseRelRep (Rep (rel (Compose Expr PGNull))) (Rep (rel (Compose Interpret PGNull))),Generic (rel (Compose Interpret PGNull)),UnpackspecRel (Rep (rel (Compose Expr PGNull))), Generic (rel Interpret), DistributeMaybe (rel (Compose Interpret PGNull)) (rel Interpret), HasFields (Rep (rel (Compose Expr PGNull)))) =>
           Selectable (rel (Compose Expr PGNull)) (Maybe (rel Interpret)) where
  queryRunner = fmap distributeMaybe (gqueryRunner :: Op.QueryRunner (rel (Compose Expr PGNull)) (rel (Compose Interpret PGNull)))

-- This lets us turn a record of Maybe's into a Maybe of fields.
class DistributeMaybe x y | x -> y where
  distributeMaybe :: x -> Maybe y

instance (Generic (rel (Compose Interpret PGNull)), Generic (rel Interpret), GDistributeMaybe (Rep (rel (Compose Interpret PGNull))) (Rep (rel Interpret))) => DistributeMaybe (rel (Compose Interpret PGNull)) (rel Interpret) where
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
          (queryRunner ***! queryRunner ***! queryRunner)

instance (Selectable e1 h1,Selectable e2 h2,Selectable e3 h3,Selectable e4 h4) => Selectable (e1,e2,e3,e4) (h1,h2,h3,h4) where
  queryRunner =
    dimap (\(a,b,c,d) -> ((a,b),(c,d)))
          (\((a,b),(c,d)) -> (a,b,c,d))
          ((queryRunner ***! queryRunner) ***! (queryRunner ***! queryRunner))

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

instance UnpackspecRel (K1 i (Compose Expr PGNull colType)) where
  unpackRel inj (K1 (Compose (Expr prim))) = void (inj prim)

--------------------------------------------------------------------------------

-- Polykinded compose. I think transformers might have this now.
data Compose (f :: l -> *) (g :: k -> l) (a :: k) = Compose (f (g a))

-- A left join takes two queries as before and the product of the left query and
-- the right query wrapped as NULL. Crucially we have a functional dependency
-- on 'ToNull' so it infers properly.
leftJoin :: (ToNull right nullRight)
         => (left -> right -> Expr PGBoolean)
         -> Op.Query left
         -> Op.Query right
         -> Op.Query (left,nullRight)
leftJoin f l r =
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

-- A single table can be transformed generically.
instance (GToNull (Rep (rel Expr)) (Rep (rel (Compose Expr PGNull))), Generic (rel Expr), Generic (rel (Compose Expr PGNull))) => ToNull (rel Expr) (rel (Compose Expr PGNull)) where
  toNull = to . gtoNull . from

class GToNull f g | f -> g where
  gtoNull :: f x -> g x

instance GToNull f f' => GToNull (M1 i c f) (M1 i c f') where
  gtoNull (M1 f) = M1 (gtoNull f)

instance (GToNull f f', GToNull g g') => GToNull (f :*: g) (f' :*: g') where
  gtoNull (f :*: g) = gtoNull f :*: gtoNull g

instance (t' ~ Col (Compose Expr PGNull) t, t' ~ Expr x) => GToNull (K1 i (Expr t)) (K1 i t') where
  gtoNull (K1 (Expr prim)) = K1 (Expr prim)

--------------------------------------------------------------------------------

-- We'll need defunctionalisation, so set the scene for that..
data TyFun :: * -> * -> *
type family Apply (f :: TyFun k1 k2 -> *) (x :: k1) :: k2

-- Define a universe of types in the database.

data PGTypes
  = PGBigint
  | PGText
  | PGBoolean
  | PGNull PGTypes
  | PGReal
  | PGTimestampWithoutTimeZone
  | PGInteger
  | PGDefault PGTypes -- ^ Used to indicate that columns may also take an extra value - `DEFAULT`.

-- Introduce a defunctionalisation symbol and mapping of database types to
-- Haskell types. Only has to be done once, and I'd include all built in
-- PostgreSQL types.

data InterpretPGType :: TyFun PGTypes * -> *

class (haskell ~ Apply InterpretPGType pg) => Lit pg haskell | haskell -> pg, pg -> haskell where
  lit :: haskell -> Expr pg

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

type instance Apply InterpretPGType 'PGTimestampWithoutTimeZone = LocalTime
instance Lit 'PGTimestampWithoutTimeZone LocalTime where
  lit = Expr . Op.unColumn . Op.pgLocalTime

type instance Apply InterpretPGType (PGNull t) = Maybe (Apply InterpretPGType t)
instance Lit t t' => Lit ('PGNull t) (Maybe t') where
  lit Nothing = Expr (Op.unColumn Op.null)
  lit (Just x) = case lit x of Expr a -> Expr a

type instance Apply InterpretPGType (PGDefault t) = Apply InterpretPGType t

-- Two functors to move from Expr (or Compose Expr PGNull) to. Store no data,
-- just used to drive the Col type family.

data InterpretNull :: k -> * where
data Interpret :: k -> * where

--------------------------------------------------------------------------------

restrict :: Op.QueryArr (Expr PGBoolean) ()
restrict = lmap (\(Expr prim) -> Op.Column prim) Op.restrict

(==.) :: Expr a -> Expr a -> Expr PGBoolean
Expr a ==. Expr b =
  case Op.Column a Op..== Op.Column b of
    Op.Column c -> Expr c

infix 4 ==.

--------------------------------------------------------------------------------

data Insertion :: k -> * where

data Default a = Default | ProvideValue a

class Insertable table row | table -> row where
  insertTable :: table -> Op.Table row ()

instance (KnownSymbol tableName,Generic (rel Insertion),Generic (rel (Table tableName)),GWriter (Rep (rel (Table tableName))) (Rep (rel Insertion))) => Insertable (rel (Table tableName)) (rel Insertion) where
  insertTable table =
    Op.Table (symbolVal (Proxy :: Proxy tableName))
             (lmap from
                   (Op.TableProperties (gwriter (from table))
                                       undefined))

class GWriter f g where
  gwriter :: f x -> Op.Writer (g x) ()

instance GWriter f f' => GWriter (M1 i c f) (M1 i c f') where
  gwriter (M1 a) =
    lmap (\(M1 a) -> a)
         (gwriter a)

instance (GWriter f f',GWriter g g') => GWriter (f :*: g) (f' :*: g') where
  gwriter (l :*: r) =
    dimap (\(l' :*: r') -> (l',r'))
          fst
          (gwriter l ***! gwriter r)

instance GWriter (K1 i (Table tableName (PGDefault t))) (K1 i (Default (Expr t))) where
  gwriter (K1 (Column columnName)) =
    dimap (\(K1 def) ->
             case def of
               Default -> Op.Column (Op.DefaultInsertExpr)
               ProvideValue (Expr a) -> Op.Column a)
          (const ())
          (Op.required columnName)

instance GWriter (K1 i (Table tableName t)) (K1 i (Expr t)) where
  gwriter (K1 (Column columnName)) =
    dimap (\(K1 (Expr e)) -> Op.Column e)
          (const ())
          (Op.required columnName)

insert
  :: Insertable table row
  => PG.Connection -> table -> [row] -> IO Int64
insert conn table rows =
  Op.runInsertMany conn
                   (insertTable table)
                   rows

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
