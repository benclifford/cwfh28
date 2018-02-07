module Main where

import qualified Database.PostgreSQL.Simple as PG
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Maybe as M
import Data.Monoid ( (<>) )
import qualified Data.Text as T

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

handleRegistrationPost :: Integer -> [(String, String)] -> S.Handler B.Html
handleRegistrationPost identifier reqBody = do
  [registration] <- liftIO $ bracket
    (PG.connectPostgreSQL "user='postgres'")
    PG.close
    $ \conn -> do
      PG.query
        conn
        "SELECT firstname, lastname, dob FROM registration WHERE id = ?"
        [identifier] :: IO [Registration]

  viewValue <- DF.postForm "Registration" (registrationDigestiveForm registration) (servantPathEnv reqBody)

  case viewValue of
    (view, Nothing) -> 
      return $ B.docTypeHtml $ do
        B.body $ do
          B.h1 $ do "Registration (there were errors): "
                    B.toHtml (show identifier)
          htmlForRegistration view

    (_, Just newRegistration) -> do
      liftIO $ bracket
        (PG.connectPostgreSQL "user='postgres'")
        PG.close
        $ \conn -> PG.execute conn 
                   "UPDATE registration SET firstname = ?, lastname = ?, dob = ? WHERE id = ?" 
                   (firstname newRegistration,
                    lastname newRegistration,
                    dob newRegistration,
                    identifier
                   )
      return "Record updated."

htmlForRegistration :: DF.View B.Html -> B.Html
htmlForRegistration view =
  B.form
    ! BA.method "post"
    $ do
      B.p $ do  "First name: "
                DB.errorList "firstname" view
                DB.inputText "firstname" view
      B.p $ do  "Last name: "
                DB.errorList "lastname" view
                DB.inputText "lastname" view
      B.p $ do  "Date of Birth: "
                DB.errorList "dob" view
                DB.inputText "dob" view
      B.p $     DB.inputSubmit "Save" 



registrationDigestiveForm :: Monad m => Registration -> DF.Form B.Html m Registration
registrationDigestiveForm initial = do
  Registration
    <$> "firstname" .: nonEmptyString (Just $ firstname initial)
    <*> "lastname" .: nonEmptyString (Just $ lastname initial)
    <*> "dob" .: dateLikeString (Just $ dob initial)


nonEmptyString def =
    (DF.check "This field must not be empty" (/= ""))
  $ DF.string def

dateLikeString def =
    (DF.check "This field must look like a date" isDateLike)
  $ nonEmptyString def

isDateLike :: String -> Bool
isDateLike s = foldr (&&) True $ map isDateChar s
  where 
    isDateChar c = c `elem` ("0123456789-/" :: String)

api :: S.Proxy API
api = S.Proxy

app = S.serve api server

server :: S.Server API
server = handlePing :<|> handleHtmlPing :<|> handleRegistration
    :<|> handleRegistrationPost


servantPathEnv :: Monad m => [(String, String)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv reqBody _ = return env
  where
      pathAsString = T.unpack . DF.fromPath
      packAsInput = DF.TextInput . T.pack
      lookupParam p = lookup (pathAsString p) reqBody
      env path = return (packAsInput <$> (M.maybeToList (lookupParam path)))

