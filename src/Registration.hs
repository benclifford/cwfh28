
module Registration where

import qualified Data.Csv as CSV
import qualified GHC.Generics as G
import qualified Generics.SOP as GS
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.SOP as PGS

data Registration = Registration {
  firstname :: String,
  lastname :: String,
  dob :: String,
  swim :: Bool
} deriving (Show, G.Generic, PG.FromRow, PG.ToRow)

instance GS.Generic Registration
instance GS.HasDatatypeInfo Registration

instance PGS.HasFieldNames Registration

