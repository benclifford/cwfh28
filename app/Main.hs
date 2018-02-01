module Main where

import Servant ( (:>) )
import qualified Servant as S

import qualified Network.Wai.Handler.Warp as W


import Lib

main :: IO ()
main = W.run 8080 app

type PingAPI = "ping" :> S.Get '[S.PlainText] String

handlePing :: S.Handler String
handlePing = return "PONG"

api :: S.Proxy PingAPI
api = S.Proxy

app = S.serve api server

server :: S.Server PingAPI
server = handlePing

