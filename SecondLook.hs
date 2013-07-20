{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Control.Applicative ((<$>))
import Data.Aeson (eitherDecode)
import Data.Text.Encoding (encodeUtf8)
import GithubPush
import Network.HTTP.Conduit (withManager)
import Network.Mail.Mime (Address(..), simpleMail)
import Network.Mail.Mime.SES (SES(..), renderSendMailSES)
import System.Environment (getEnv)
import Yesod

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

data SecondLook = SecondLook

mkYesod "SecondLook" [parseRoutes|
    / RootR POST
|]

instance Yesod SecondLook

data PostError = MissingPayload -- Missing post parameter "payload"
               | BadPayload     -- Unparseable parameter "payload"
               deriving (Show)


postRootR :: Handler TypedContent
postRootR =
    getRequest >>= \_ ->
    lookupPostParam "payload" >>=
    \case
        Just push -> handleGithubPush push >>
                    respond "text/plain" ("ok" :: T.Text)
        Nothing   -> liftIO (sendErrorEmail MissingPayload "Bad Request") >>
                    invalidArgs ["Expected parameter 'payload'"]

handleGithubPush :: T.Text -> Handler ()
handleGithubPush push =
    liftIO (putStrLn $ T.unpack push) >>
    decodeGithubPush push             >>
    return ()

decodeGithubPush :: T.Text -> Handler GithubPush
decodeGithubPush push =
    case eitherDecode (textToBS push) of
        Right payload -> return payload
        Left  err     -> do
            liftIO $ sendErrorEmail BadPayload (textSTL push)
            invalidArgs [ "Invalid payload"
                        , "Aeson parse error: " `T.append` T.pack err
                        ]
  where
    textToBS :: T.Text -> BSL.ByteString
    textToBS text = BSL.fromChunks [encodeUtf8 text]

    -- "Text Strict-to-Lazy"
    textSTL :: T.Text -> TL.Text
    textSTL strict = TL.fromChunks [strict]

sendErrorEmail :: PostError -> TL.Text -> IO ()
sendErrorEmail err request = withManager $ \manager ->
    liftIO getSes >>= \ses ->
    liftIO (simpleMail from to subject plainBody htmlBody attachments) >>=
    renderSendMailSES manager ses
  where
    from :: Address
    from = Address (Just "SecondLook")     "mitchellwrosen@gmail.com"

    to :: Address
    to = Address (Just "Mitchell Rosen") "mitchellwrosen@gmail.com"

    subject :: T.Text
    subject = "SecondLook error"

    plainBody :: TL.Text
    plainBody = TL.unlines ["Bad request: " `TL.append` (TL.pack . show) err
                           , request
                           ]

    htmlBody :: TL.Text
    htmlBody = TL.unlines ["Bad request: " `TL.append` (TL.pack . show) err
                          , request
                          ]

    attachments :: [(T.Text, FilePath)]
    attachments = []

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

main :: IO ()
main = do
    access_key <- getEnv "SECOND_LOOK_ACCESS_KEY"
    secret_key <- getEnv "SECOND_LOOK_SECRET_KEY"
    putStrLn $ "Access key = " ++ access_key
    putStrLn $ "Secret key = " ++ secret_key
    warp 8080 SecondLook
