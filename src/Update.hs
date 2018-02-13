{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language RankNTypes #-}

module Update where

import Control.Monad.IO.Class (liftIO)

import Data.Int (Int64)
import Data.String (fromString)
import Data.List (intercalate)
import Data.Monoid ( (<>) )

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.SOP as PGS

import qualified Generics.SOP as GS

gupdateInto :: forall r. forall s.
    (PG.ToRow r, PGS.HasFieldNames r,
     PG.ToRow s)
  => PG.Connection -> PG.Query -> PG.Query -> r -> s -> IO Int64
gupdateInto conn tbl whereclause val where_val = do
  let fieldNames = PGS.fieldNames $ (GS.Proxy :: GS.Proxy r)
  let fieldSets = map (\name -> name <> " = ?") fieldNames

  let sql = ("UPDATE " <> tbl
          <> " SET " <> (fromString $ intercalate ", " fieldSets)
          <> " WHERE " <> whereclause)

  PG.execute conn sql (val PG.:. where_val)


