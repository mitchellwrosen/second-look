{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Email where

import Control.Applicative ((<$>))
import Control.Exception (catch)
import Control.Monad.Trans (liftIO)
import Network.HTTP.Conduit (HttpException(..), withManager)
import Network.Mail.Mime (Address(..), Mail, mailBcc, mailCc, mailTo, simpleMail)
import Network.Mail.Mime.SES (SES(..), renderSendMailSES)
import System.Environment (getEnv)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy        as TL

import Data.Text.Encoding.Extras (t2bs)

instance Show Mail
instance Show SES

sendEmail :: Mail -> IO ()
sendEmail mail = do
    catch (withManager $ \manager -> do
              let recipients = extractRecipientsFromMail mail
              ses <- liftIO $ getSes recipients
              renderSendMailSES manager ses mail)
          (\(ex :: HttpException) -> do
              sendErrorEmail [ "HttpException", TL.pack $ show ex])

extractRecipientsFromMail :: Mail -> [BS.ByteString]
extractRecipientsFromMail mail =
    concatMap extractRecipientsFromAddresses [ mailTo  mail
                                             , mailCc  mail
                                             , mailBcc mail
                                             ]

extractRecipientsFromAddresses :: [Address] -> [BS.ByteString]
extractRecipientsFromAddresses = map (t2bs . addressEmail)

getSes :: [BS.ByteString] -> IO SES
getSes recipients = do
    access_key <- BS.pack <$> getEnv "SECOND_LOOK_ACCESS_KEY" -- Uncaught errors, but they are called first in main.
    secret_key <- BS.pack <$> getEnv "SECOND_LOOK_SECRET_KEY"
    return $ SES
        { sesFrom      = "mitchellwrosen@gmail.com"
        , sesTo        = recipients
        , sesAccessKey = access_key
        , sesSecretKey = secret_key
        }

sendErrorEmail :: [TL.Text] -> IO ()
sendErrorEmail message =
    simpleMail to from subject plainBody htmlBody attachments >>=
    sendEmail
  where
    to          = Address (Just "Mitchell Rosen") "mitchellwrosen@gmail.com"
    from        = Address (Just "Second Look")    "mitchellwrosen@gmail.com"
    subject     = "Second Look error"
    plainBody   = TL.unlines message
    htmlBody    = TL.unlines message
    attachments = []
