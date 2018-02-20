module Config where

import qualified GHC.Generics as G
import qualified Data.Yaml as Y
import qualified Network.Socket as N

import Orphans

data Config = Config {
    urlbase :: String
  , smtpFrom :: String
  , smtpServer :: String
  , smtpPort :: N.PortNumber
  , smtpUser :: String
  , smtpPassword :: String

} deriving (G.Generic, Y.FromJSON)

getConfig :: IO Config
getConfig = do
  e <- Y.decodeFileEither "config.yaml"
  either
    (\err -> error $ "cannot read config file: " ++ show err)
    return
    e

