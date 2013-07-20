{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Email where

import Control.Applicative ((<$>))
import Control.Exception (catch)
import Control.Monad.Trans (liftIO)
import Network.HTTP.Conduit (HttpException(..), withManager)
import Network.Mail.Mime (Address(..), Mail, simpleMail)
import Network.Mail.Mime.SES (SES(..), renderSendMailSES)
import System.Environment (getEnv)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy        as TL

sendEmail :: Mail -> IO ()
sendEmail mail = 
    catch (withManager $ \manager -> do
              ses <- liftIO getSes
              renderSendMailSES manager ses mail)
          (\(ex :: HttpException) -> do
              sendErrorEmail $ TL.pack $ show ex)

-- Unchecked getEnv errors, but they are called first in main.
getSes :: IO SES
getSes = do
    access_key <- BS.pack <$> getEnv "SECOND_LOOK_ACCESS_KEY"
    secret_key <- BS.pack <$> getEnv "SECOND_LOOK_SECRET_KEY"
    return $ SES
        { sesFrom      = "mitchellwrosen@gmail.com"
        , sesTo        = ["mitchellwrosen@gmail.com"]
        , sesAccessKey = access_key
        , sesSecretKey = secret_key
        }

-- | Sends a "debug me now" email to myself.
sendErrorEmail :: TL.Text    -- | The text to include in the error email.
               -> IO ()
sendErrorEmail message =
    simpleMail to from subject plainBody htmlBody attachments >>=
    sendEmail
  where
    to          = Address (Just "Mitchell Rosen") "mitchellwrosen@gmail.com"
    from        = Address (Just "SecondLook")     "mitchellwrosen@gmail.com"
    subject     = "SecondLook error"
    plainBody   = message
    htmlBody    = message
    attachments = []
