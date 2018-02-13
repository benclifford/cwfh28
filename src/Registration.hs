
module Registration where

import qualified Data.Csv as CSV
import qualified GHC.Generics as G
import qualified Database.PostgreSQL.Simple as PG

data Registration = Registration {
  firstname :: String,
  lastname :: String,
  dob :: String,
  swim :: Bool
} deriving (Show, G.Generic, PG.FromRow)
