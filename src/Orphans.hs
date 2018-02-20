module Orphans where

import qualified Data.Csv as CSV
import qualified Network.Socket as N
import qualified Data.Yaml as Y

instance CSV.ToField Bool
  where
    toField bool = CSV.toField (show bool)

instance Y.FromJSON N.PortNumber
  where parseJSON v = read <$> Y.parseJSON v

