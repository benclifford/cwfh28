{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
module Orphans where

import qualified Data.Csv as CSV
import Database.PostgreSQL.Simple.Time as PGT
import qualified Network.Socket as N
import qualified Data.Yaml as Y

instance CSV.ToField Bool
  where
    toField bool = CSV.toField (show bool)

instance CSV.ToField PGT.ZonedTimestamp
  where
    toField time = CSV.toField (show time)

instance Y.FromJSON N.PortNumber
  where parseJSON v = read <$> Y.parseJSON v

