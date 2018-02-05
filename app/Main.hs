module Main where

import qualified Database.PostgreSQL.Simple as PG
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)

import Servant.HTML.Blaze as SB
import qualified Text.Blaze.Html5 as B

import Servant ( (:>), ( :<|> )(..) )
import qualified Servant as S

import qualified Network.Wai.Handler.Warp as W


import Lib
import Registration

main :: IO ()
main = W.run 8080 app

type PingAPI = "ping" :> S.Get '[S.PlainText] String

type HtmlPingAPI = "htmlping" :> S.Get '[SB.HTML] B.Html

type RegistrationAPI = "registration" :> S.Capture "id" Integer :> S.Get '[SB.HTML] B.Html

type API = PingAPI
      :<|> HtmlPingAPI
      :<|> RegistrationAPI

handlePing :: S.Handler String
handlePing = return "PONG"

handleHtmlPing :: S.Handler B.Html
handleHtmlPing = return $ B.docTypeHtml $ do
  B.head $ do
    B.title "HTMLPONG"
  B.body $ do
    B.h1 "HTML Ping Response"
    B.p "It seems to work ok"

handleRegistration :: Integer -> S.Handler B.Html
handleRegistration identifier = do
  [registration] <- liftIO $ bracket
    (PG.connectPostgreSQL "user='postgres'")
    PG.close
    $ \conn -> do
      PG.query
        conn
        "SELECT firstname, lastname, dob FROM registration WHERE id = ?"
        [identifier] :: IO [Registration]

  return $ B.docTypeHtml $ do
    B.body $ do
      B.h1 $ do "Registration "
                B.toHtml (show identifier)
      B.p $ do  "First name: "
                B.toHtml (firstname registration)
      B.p $ do  "Last name: "
                B.toHtml (lastname registration)
      B.p $ do  "Date of Birth: "
                B.toHtml (dob registration)

api :: S.Proxy API
api = S.Proxy

app = S.serve api server

server :: S.Server API
server = handlePing :<|> handleHtmlPing :<|> handleRegistration

