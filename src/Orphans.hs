module Orphans where

import qualified Data.Csv as CSV

instance CSV.ToField Bool
  where
    toField bool = CSV.toField (show bool)
