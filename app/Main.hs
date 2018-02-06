module Main where

import qualified Database.PostgreSQL.Simple as PG
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)

import Data.Monoid ( (<>) )

import Servant.HTML.Blaze as SB
import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import qualified Text.Digestive as DF
import Text.Digestive ( (.:) )
import Text.Digestive.Blaze.Html5 as DB

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

type RegistrationPostAPI = "registration" :> S.Capture "id" Integer :> S.ReqBody '[S.FormUrlEncoded] [(String,String)] :> S.Post '[HTML] B.Html


type API = PingAPI
      :<|> HtmlPingAPI
      :<|> RegistrationAPI
      :<|> RegistrationPostAPI

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

  view <- DF.getForm "Registration" (registrationDigestiveForm registration)

  return $ B.docTypeHtml $ do
    B.body $ do
      B.h1 $ do "Registration "
                B.toHtml (show identifier)
      htmlForRegistration view

handleRegistrationPost = error "We'll implement this later"

htmlForRegistration :: DF.View B.Html -> B.Html
htmlForRegistration view =
  B.form
    ! BA.method "post"
    $ do
      B.p $ do  "First name: "
                DB.inputText "firstname" view
      B.p $ do  "Last name: "
                DB.inputText "lastname" view
      B.p $ do  "Date of Birth: "
                DB.inputText "dob" view
      B.p $     DB.inputSubmit "Save" 



registrationDigestiveForm :: Monad m => Registration -> DF.Form B.Html m Registration
registrationDigestiveForm initial = do
  Registration
    <$> "firstname" .: DF.string (Just $ firstname initial)
    <*> "lastname" .: DF.string (Just $ lastname initial)
    <*> "dob" .: DF.string (Just $ dob initial)



api :: S.Proxy API
api = S.Proxy

app = S.serve api server

server :: S.Server API
server = handlePing :<|> handleHtmlPing :<|> handleRegistration
    :<|> handleRegistrationPost


