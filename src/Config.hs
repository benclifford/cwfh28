module Config where

import qualified GHC.Generics as G
import qualified Data.Yaml as Y

data Config = Config {
  urlbase :: String
} deriving (G.Generic, Y.FromJSON)

getConfig :: IO Config
getConfig = do
  e <- Y.decodeFileEither "config.yaml"
  either
    (\err -> error $ "cannot read config file: " ++ show err)
    return
    e

