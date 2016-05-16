{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Opaleye where

import Control.Exception (fromException)
import Control.Monad.Catch (MonadMask, mask, try, throwM)
import Control.Monad.Free.Church (F, liftF, iterM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader
import Data.Int (Int64)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import GHC.Generics (Generic, Rep)
import Opaleye.TF
       (Selectable, Insertable, Query, Insertion, Interpret, Expr,
        ColumnView, ExtractSchema, PGType(PGBoolean))
import Opaleye.TF.Scope
import qualified Opaleye.TF as Op

--------------------------------------------------------------------------------
-- | Syntax tree for running individual statements against a database.

data StatementSyntax :: * -> * where
        Select ::
            Selectable pg haskell =>
            Query 'Z pg -> ([haskell] -> k) -> StatementSyntax k
        Insert1Returning ::
            (Insertable (rel Insertion),
             Selectable (rel (Expr 'Z)) (rel Interpret),
             ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr 'Z))),
             Generic (rel (Expr 'Z))) =>
            (rel Insertion) -> (rel Interpret -> k) -> StatementSyntax k
        Insert ::
            Insertable (rel Insertion) =>
            [rel Insertion] -> (Int64 -> k) -> StatementSyntax k
        Update ::
            (Insertable (rel (Expr 'Z)),
             ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr 'Z))),
             Generic (rel (Expr 'Z))) =>
            (rel (Expr 'Z) -> Expr 'Z 'PGBoolean) ->
              (rel (Expr 'Z) -> rel (Expr 'Z)) -> (Int64 -> k) -> StatementSyntax k

deriving instance Functor StatementSyntax

-- | The class of monads that can run database statements.
class Monad m => MonadStatement m where
  liftStatements :: F StatementSyntax a -> m a

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadStatement m) => MonadStatement (t m) where
  liftStatements = lift . liftStatements

select :: (MonadStatement m,Selectable pg haskell)
       => Query 'Z pg -> m [haskell]
select q = liftStatements (liftF (Select q id))

insert1Returning
  :: (MonadStatement m,Insertable (rel Insertion),Selectable (rel (Expr 'Z)) (rel Interpret),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr 'Z))),Generic (rel (Expr 'Z)))
  => (rel Insertion) -> m (rel Interpret)
insert1Returning row = liftStatements (liftF (Insert1Returning row id))

insert
  :: MonadStatement m
  => Insertable (rel Insertion) => [rel Insertion] -> m Int64
insert rows = liftStatements (liftF (Insert rows id))

update :: (MonadStatement m,Insertable (rel (Expr 'Z)),ColumnView (Rep (rel ExtractSchema)) (Rep (rel (Expr 'Z))),Generic (rel (Expr 'Z)))
       => (rel (Expr 'Z) -> Expr 'Z 'PGBoolean)
       -> (rel (Expr 'Z) -> rel (Expr 'Z))
       -> m Int64
update where_ up = liftStatements (liftF (Update where_ up id))

--------------------------------------------------------------------------------
-- | The class of monads that can perform a rollback, aborting a transaction.
class Monad m => MonadRollback e m | m -> e where
  abortTransaction :: e -> m a

--------------------------------------------------------------------------------
-- | The syntax of programs that can run transactions.
data TransactionSyntax e k =
  forall a.
  Statements Pg.TransactionMode
             (Pg.SqlError -> Bool)
             (forall m. (MonadStatement m, MonadRollback e m) => m a)
             (Either e a -> k)

deriving instance Functor (TransactionSyntax e)

-- | The class of monads that can run transactions.
class Monad m => MonadTransaction m where
  liftTransaction :: F (TransactionSyntax e) a -> m a

runTransaction :: MonadTransaction m
               => Pg.TransactionMode
               -> (Pg.SqlError -> Bool)
               -> (forall n. (MonadStatement n, MonadRollback e n) => n a)
               -> m (Either e a)
runTransaction mode shouldRetry t =
  liftTransaction (liftF (Statements mode shouldRetry t id))

runSerializableTransaction
  :: MonadTransaction m
  => (forall n. (MonadStatement n, MonadRollback e n) => n a) -> m (Either e a)
runSerializableTransaction =
  runTransaction
    Pg.TransactionMode {Pg.isolationLevel = Pg.Serializable
                       ,Pg.readWriteMode = Pg.ReadWrite}
    Pg.isSerializationError

--------------------------------------------------------------------------------
newtype PostgreSQLTransactionT m a =
  PostgreSQLTransactionT (ReaderT Pg.Connection m a)
  deriving (Functor,Applicative,Monad)

instance (MonadIO m,MonadMask m) => MonadTransaction (PostgreSQLTransactionT m) where
  liftTransaction =
    PostgreSQLTransactionT .
    iterM (\(Statements mode shouldRetry t k) ->
             do pg <- ask
                out <-
                  withTransactionModeRetry
                    mode
                    shouldRetry
                    pg
                    (do out <- runPostgreSQLStatements t pg
                        case out of
                          Left{} -> liftIO (Pg.rollback pg)
                          Right{} -> return ()
                        return out)
                k out)

-- | A handler that runs individual statements against a PostgreSQL connection.
-- Again, we just use a reader monad to pass the connection handle around.
newtype PostgreSQLStatementT e m a =
  PostgreSQLStatementT (ExceptT e (ReaderT Pg.Connection m) a)
  deriving (Functor,Applicative,Monad)

runPostgreSQLStatements
  :: PostgreSQLStatementT e m a -> Pg.Connection -> m (Either e a)
runPostgreSQLStatements (PostgreSQLStatementT r) = runReaderT (runExceptT r)

instance (MonadIO m) => MonadStatement (PostgreSQLStatementT e m) where
  liftStatements =
    PostgreSQLStatementT .
    iterM (\op ->
             do conn <- lift ask
                step conn op)
    where step pg (Select q k) = liftIO (Op.select pg q) >>= k
          step pg (Insert1Returning q k) =
            liftIO (Op.insert1Returning pg q) >>= k
          step pg (Insert q k) = liftIO (Op.insert pg q) >>= k
          step pg (Update a b k) = liftIO (Op.update pg a b) >>= k

instance (Monad m) => MonadRollback e (PostgreSQLStatementT e m) where
  abortTransaction l = PostgreSQLStatementT (ExceptT (return (Left l)))

runPostgreSQLTransactions
  :: PostgreSQLTransactionT m a -> Pg.Connection -> m a
runPostgreSQLTransactions (PostgreSQLTransactionT r) = runReaderT r

--------------------------------------------------------------------------------
withTransactionModeRetry :: (MonadMask m,MonadIO m)
                         => Pg.TransactionMode
                         -> (Pg.SqlError -> Bool)
                         -> Pg.Connection
                         -> m a
                         -> m a
withTransactionModeRetry mode shouldRetry conn act =
  mask $
  \restore ->
    retryLoop $
    try $
    do a <- restore act
       liftIO (Pg.commit conn)
       return a
  where retryLoop act' =
          do liftIO (Pg.beginMode mode conn)
             r <- act'
             case r of
               Left e ->
                 do liftIO (Pg.rollback conn)
                    case fmap shouldRetry (fromException e) of
                      Just True -> retryLoop act'
                      _ -> throwM e
               Right a -> return a
