{-# Language OverloadedStrings #-}

module DB where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Time as PGT
import qualified Database.PostgreSQL.Simple.SOP as PGS

import Registration

withDB :: MonadIO io => (PG.Connection -> IO b) -> io b
withDB act = liftIO $ bracket
  (PG.connectPostgreSQL "user='postgres'")
  PG.close
  act

selectByNonce :: MonadIO io => String -> io Registration
selectByNonce identifier = do
  res <- withDB $ \conn -> PGS.gselectFrom conn "registration where nonce = ?" [identifier]
  case res of 
    [r] -> return r
    [] -> error $ "selectByNonce: no rows returned for " ++ identifier
    _ -> error $ "selectByNonce: multiple rows returned for " ++ identifier

generateOCC :: IO PGT.ZonedTimestamp
generateOCC = do
  [[n]] <- withDB $ \conn -> PG.query conn "SELECT NOW()" ()
  return n

