{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Int (Int32)
import Data.Monoid ((<>))
import GHC.Generics
import Opaleye.TF
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
data Table f =
  Table {tableFoo :: Col f ('Column "foo" ('NotNullable PGInteger))
        ,tableBar :: Col f ('Column "bar" ('Nullable 'PGText))}
  deriving ((Generic))

type instance TableName Table = "table"

canQueryTable :: TestTree
canQueryTable =
  testCase "queryTable" $
  let query :: Query (Table Expr)
      query = queryTable
  in shouldTypecheck (\pg -> select pg query :: IO [Table Interpret])

canSelectASingleColumn :: TestTree
canSelectASingleColumn =
  testCase "Can select a single Expr" $
  let query :: Query (Expr 'PGInteger)
      query = fmap tableFoo queryTable
  in shouldTypecheck (\pg -> select pg query :: IO [Int32])

canOrder :: TestTree
canOrder =
  testCase "Can order" $
  let query :: Query (Table Expr)
      query =
        orderBy (asc tableFoo <> orderNulls asc NullsLast tableBar) queryTable
  in shouldTypecheck (\pg -> select pg query :: IO [Table Interpret])

leftJoinMonadic :: TestTree
leftJoinMonadic = testCase "Monadic left join" $
  let query :: Query s (Table (Expr s),  Table (Compose (Expr s) 'Nullable))
      query = do
        t1 <- queryTable
        t2 <- leftJoinTableOn (\t2 -> tableFoo t2 ==. tableFoo t1)
        return (t1, t2)
  in shouldTypecheck (\pg -> select pg query :: IO [(Table Interpret, Maybe (Table Interpret))])

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain (testGroup "Tests" [canQueryTable,canSelectASingleColumn])

shouldTypecheck :: a -> IO ()
shouldTypecheck _ = return ()
