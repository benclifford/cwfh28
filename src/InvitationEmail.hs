{-# Language OverloadedStrings #-}
module InvitationEmail where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Network.HaskellNet.SMTP.SSL as SSL
import qualified Network.Mail.Mime as M

import Config

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

