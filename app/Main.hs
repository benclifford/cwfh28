module Main where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.SOP as PGS

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Csv as CSV
import qualified Data.Maybe as M
import Data.Monoid ( (<>) )
import Data.String (fromString)
import qualified Data.Text as T

import System.Random (randomRIO)

import Servant.CSV.Cassava as SC
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

import Config
import DB
import InvitationEmail
import qualified Invitation as I
import Lib
import Orphans
import Registration
import Update

main :: IO ()
main = W.run 8080 app

type PingAPI = "ping" :> S.Get '[S.PlainText] String

type HtmlPingAPI = "htmlping" :> S.Get '[SB.HTML] B.Html

type RegistrationAPI = "registration" :> S.Capture "id" String :> S.Get '[SB.HTML] B.Html

type RegistrationPostAPI = "registration" :> S.Capture "id" String :> S.ReqBody '[S.FormUrlEncoded] [(String,String)] :> S.Post '[HTML] B.Html

type CSVAPI = "admin" :> "csv" :> S.Get '[SC.CSV] (S.Headers '[S.Header "Content-Disposition" String] [Registration])

type InvitationGetAPI = "admin" :> "invite" :> S.Get '[SB.HTML] B.Html
type InvitationPostAPI = "admin" :> "invite" :> S.ReqBody '[S.FormUrlEncoded] [(String,String)] :> S.Post '[SB.HTML] B.Html

type MailTestAPI = "admin" :> "mailtest" :> S.Get '[SB.HTML] B.Html

type API = PingAPI
      :<|> HtmlPingAPI
      :<|> RegistrationAPI
      :<|> RegistrationPostAPI
      :<|> CSVAPI
      :<|> InvitationGetAPI
      :<|> InvitationPostAPI
      :<|> MailTestAPI

handlePing :: S.Handler String
handlePing = return "PONG"

handleHtmlPing :: S.Handler B.Html
handleHtmlPing = return $ B.docTypeHtml $ do
  B.head $ do
    B.title "HTMLPONG"
  B.body $ do
    B.h1 "HTML Ping Response"
    B.p "It seems to work ok"

handleRegistration :: String -> S.Handler B.Html
handleRegistration identifier = do
  registration <- selectByNonce identifier

  if (editableStatus . status) registration
  then handleEditableRegistration registration
  else handleReadOnlyRegistration registration

handleEditableRegistration :: Registration -> S.Handler B.Html
handleEditableRegistration registration = do

  view <- DF.getForm "Registration" (registrationDigestiveForm registration)

  return $ B.docTypeHtml $ do
    B.body $ do
      B.h1 $ do "Registration "
                B.toHtml (show $ nonce registration)
      htmlForRegistration view

handleReadOnlyRegistration :: Registration -> S.Handler B.Html
handleReadOnlyRegistration registration =
  return $ B.docTypeHtml $ do
    B.body $ do
      B.h1 $ do "Completed registration " 
                B.toHtml (show $ nonce registration)
      B.p $ "First name: " <> (fromString . firstname) registration
      B.p $ "Last name: " <> (fromString . lastname) registration
      B.p $ "Date of birth: " <> (fromString . dob) registration

editableStatus :: String -> Bool
editableStatus "N" = True
editableStatus "C" = False
editableStatus _ = False

handleRegistrationPost :: String -> [(String, String)] -> S.Handler B.Html
handleRegistrationPost identifier reqBody = do
  registration <- selectByNonce identifier

  viewValue <- DF.postForm "Registration" (registrationDigestiveForm registration) (servantPathEnv reqBody)

  case viewValue of
    (view, Nothing) -> 
      return $ B.docTypeHtml $ do
        B.body $ do
          B.h1 $ do "Registration (there were errors): "
                    B.toHtml (show identifier)
          htmlForRegistration view

    (_, Just newRegistration) -> do
      let oldOcc = occ newRegistration
      newOcc <- liftIO $ generateOCC
      let n = newRegistration { status = "C", occ = newOcc}
      rows <- withDB $ \conn -> gupdateInto conn "registration" "nonce = ? AND occ = ?" n (identifier, occ newRegistration)
      when (rows /= 1) $ error "Registration POST did not update exactly one row"
      return "Record updated."


htmlForRegistration :: DF.View B.Html -> B.Html
htmlForRegistration view =
  B.form
    ! BA.method "post"
    $ do
      DB.inputHidden "occ" view
      DB.errorList "occ" view
      B.p $ do  "First name: "
                DB.errorList "firstname" view
                DB.inputText "firstname" view
      B.p $ do  "Last name: "
                DB.errorList "lastname" view
                DB.inputText "lastname" view
      B.p $ do  "Date of Birth: "
                DB.errorList "dob" view
                DB.inputText "dob" view
      B.p $ do  "Can participant swim?: "
                DB.errorList "swim" view
                DB.inputCheckbox "swim" view
      B.p $     DB.inputSubmit "Save" 



registrationDigestiveForm :: Monad m => Registration -> DF.Form B.Html m Registration
registrationDigestiveForm initial = do
  Registration
    <$> "firstname" .: nonEmptyString (Just $ firstname initial)
    <*> "lastname" .: nonEmptyString (Just $ lastname initial)
    <*> "dob" .: dateLikeString (Just $ dob initial)
    <*> "swim" .: DF.bool (Just $ swim initial)
    <*> (pure . nonce) initial
    <*> "occ" .: occVersion (occ initial)
    <*> "email" .: DF.optionalString (email initial)
    <*> (pure . status) initial

occVersion dbOCC = 
  DF.check 
    "This form has been modified by someone else - please reload" 
    (\newOCC -> show newOCC == show dbOCC)
  $ DF.stringRead "OCC Version" (Just $ dbOCC)

handleInvitationGet :: S.Handler B.Html
handleInvitationGet = do
  view <- DF.getForm "Invitation" invitationDigestiveForm
  return $ B.docTypeHtml $ do
    B.body $ do
      B.h1 $ "New Invitation"
      htmlForInvitation view

handleInvitationPost :: [(String, String)] -> S.Handler B.Html
handleInvitationPost reqBody = do

  viewValue <- DF.postForm "Invitation" invitationDigestiveForm (servantPathEnv reqBody)

  case viewValue of
    (view, Nothing) -> 
      return $ B.docTypeHtml $ do
        B.body $ do
          B.h1 $ "New Invitation (there were errors): "
          htmlForInvitation view

    (_, Just newInvitation) -> liftIO $ doInvitation newInvitation

htmlForInvitation :: DF.View B.Html -> B.Html
htmlForInvitation view = 
  B.form
    ! BA.method "post"
    $ do
      B.p $ do  "First name: "
                DB.errorList "firstname" view
                DB.inputText "firstname" view
      B.p $ do  "Last name: "
                DB.errorList "lastname" view
                DB.inputText "lastname" view
      B.p $ do  "email: "
                DB.errorList "email" view
                DB.inputText "email" view
      B.p $     DB.inputSubmit "Save" 

invitationDigestiveForm :: Monad m => DF.Form B.Html m I.Invitation
invitationDigestiveForm =
  I.Invitation
    <$> "firstname" .: nonEmptyString Nothing
    <*> "lastname" .: nonEmptyString Nothing
    <*> "email" .: nonEmptyString Nothing

doInvitation :: I.Invitation -> IO B.Html
doInvitation invitation = do

  config <- getConfig

  newNonce <- generateNonce

  newOCC <- generateOCC

  let registration = Registration {
    firstname = I.firstname invitation,
    lastname = I.lastname invitation,
    dob = "",
    swim = False,
    nonce = Just newNonce,
    occ = newOCC,
    email = Just (I.email invitation),
    status = "N"
    
  }

  withDB $ \conn -> do
      PGS.ginsertInto conn "registration" registration

  let url = (urlbase config) ++ "/registration/" ++ newNonce

  sendInvitationEmail newNonce

  return $ B.docTypeHtml $ do
    B.head $ do
      B.title "Invitation Processed"
    B.body $ do
      B.h1 "Invitation processed."
      B.p $ "Please ask the participant to complete the form at "
            <> (B.a ! BA.href (fromString url)) (fromString url)
                   
generateNonce :: IO String
generateNonce = sequence $ take 32 $ repeat $ randomRIO ('a', 'z')

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
    :<|> handleRegistrationPost :<|> handleCSV
    :<|> handleInvitationGet :<|> handleInvitationPost
    :<|> handleMailTest


servantPathEnv :: Monad m => [(String, String)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv reqBody _ = return env
  where
      pathAsString = T.unpack . DF.fromPath
      packAsInput = DF.TextInput . T.pack
      lookupParam p = lookup (pathAsString p) reqBody
      env path = return (packAsInput <$> (M.maybeToList (lookupParam path)))



handleCSV :: S.Handler (S.Headers '[S.Header "Content-Disposition" String] [Registration])
handleCSV = do
  rs <- withDB $ \conn ->
      PGS.gselectFrom conn "registration" ()
  return $ S.addHeader "attachment;filename=\"registrations.csv\"" rs

instance CSV.ToNamedRecord Registration
instance CSV.DefaultOrdered Registration


handleMailTest = do
  liftIO $ sendTestMail
  return $ B.p "mail test sent"

