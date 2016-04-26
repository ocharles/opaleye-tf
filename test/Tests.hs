{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Int (Int32)
import GHC.Generics
import Opaleye.TF
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
data Table f =
  Table {tableFoo :: Col f ('Column "foo" 'PGInteger)}
  deriving (Generic)

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

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain (testGroup "Tests" [canQueryTable,canSelectASingleColumn])

shouldTypecheck :: a -> IO ()
shouldTypecheck _ = return ()
