{-# Language OverloadedStrings #-}
module InvitationEmail where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Monoid ( (<>) )
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.SOP as PGS
import qualified Network.HaskellNet.SMTP.SSL as SSL
import qualified Network.Mail.Mime as M
import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Blaze.Html.Renderer.Text as BT

import Config
import DB
import Registration

sendInvitationEmail identifier = do
  c <- getConfig

  r <- selectByNonce identifier

  let ub = urlbase c
  let url = ub <> "/registration/" <> identifier

  let fullname = T.pack $ firstname r <> " " <> lastname r
  let targetEmail = T.pack $
        fromMaybe
          (error $ "No email supplied for " <> identifier)
          (email r)
  let subject = "Registration form for " <> fullname

  let plaintext = 
          "Hello.\n"
       <> "Please complete this registration form.\n"
       <> TL.pack url
 
  let htmltext = BT.renderHtml $ do
        B.p "Hello"
        B.p "Please complete this registration form.\n"
        B.p $ (B.a ! BA.href (fromString url)) (fromString url)

  let mail = M.Mail {
      M.mailFrom = M.Address { M.addressName = Just "Registration System"
                             , M.addressEmail = T.pack $ smtpFrom c
                             }
    , M.mailTo = [M.Address { M.addressName = Just fullname
                            , M.addressEmail = targetEmail
                            }
             ]
    , M.mailCc = []
    , M.mailBcc = []
    , M.mailHeaders = [("Subject", subject)]
    , M.mailParts = [[M.plainPart plaintext, M.htmlPart htmltext]]
    }

  sendEmail mail


sendTestMail = do
  c <- getConfig
  sendEmail $
    M.Mail {
      M.mailFrom = M.Address { M.addressName = Just "Registration System"
                             , M.addressEmail = T.pack $ smtpFrom c
                             }
    , M.mailTo = [M.Address { M.addressName = Just "You"
                            , M.addressEmail = T.pack $ smtpFrom c
                            }
             ]
    , M.mailCc = []
    , M.mailBcc = []
    , M.mailHeaders = [("Subject", "Test registration invitation for you")]
    , M.mailParts = [[M.plainPart "HELLO"]]
    }

sendEmail :: M.Mail -> IO ()
sendEmail msg = do
  config <- getConfig

  rendered <- BSL.toStrict <$> M.renderMail' msg

  let sslSettings = SSL.defaultSettingsSMTPSTARTTLS
       { SSL.sslPort = smtpPort config,
         SSL.sslDisableCertificateValidation = True
       }

  SSL.doSMTPSTARTTLSWithSettings
    (smtpServer config)
    sslSettings
    $ \connection -> do
      succeeded  <- SSL.authenticate SSL.LOGIN
                                     (smtpUser config)
                                     (smtpPassword config)
                                     connection
      if succeeded 
      then
          SSL.sendMail (addressToText (M.mailFrom msg))
                       (map addressToText (M.mailTo msg))
                       rendered connection
      else error "Could not authenticate to SMTP server"

addressToText = T.unpack . M.addressEmail

