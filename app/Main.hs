module Main where

import Servant.HTML.Blaze as SB
import qualified Text.Blaze.Html5 as B

import Servant ( (:>), ( :<|> )(..) )
import qualified Servant as S

import qualified Network.Wai.Handler.Warp as W


import Lib

main :: IO ()
main = W.run 8080 app

type PingAPI = "ping" :> S.Get '[S.PlainText] String

type HtmlPingAPI = "htmlping" :> S.Get '[SB.HTML] B.Html

type API = PingAPI
      :<|> HtmlPingAPI

handlePing :: S.Handler String
handlePing = return "PONG"

handleHtmlPing :: S.Handler B.Html
handleHtmlPing = return $ B.docTypeHtml $ do
  B.head $ do
    B.title "HTMLPONG"
  B.body $ do
    B.h1 "HTML Ping Response"
    B.p "It seems to work ok"

api :: S.Proxy API
api = S.Proxy

app = S.serve api server

server :: S.Server API
server = handlePing :<|> handleHtmlPing

