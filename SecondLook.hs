{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import System.Environment (getEnv)

import Control.Monad.Trans (liftIO)
import Control.Lens ((^.))
import Data.Aeson (eitherDecode)
import Github.Users (detailedOwnerEmail, userInfoFor)
import Network.Mail.Mime (Address(..), simpleMail)
import System.Directory (getPermissions, readable)
import Text.Hastache (MuType(..), defaultConfig, hastacheFile)
import Text.Hastache.Context (mkStrContext)
import Text.Regex.PCRE ((=~))
import Yesod.Core (Yesod)
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Dispatch (mkYesod, parseRoutes, warp)
import Yesod.Core.Handler (getRequest, invalidArgs, lookupPostParam, respond)
import Yesod.Routes.Class (renderRoute)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

import Email (sendEmail, sendErrorEmail)
import GithubPayload
import Text.Extras (bs2ts, bsl2tl, tl2bsl, ts2tl)

data SecondLook = SecondLook

mkYesod "SecondLook" [parseRoutes|
    / RootR POST
|]

instance Yesod SecondLook

-- | Handler for POSTs to /. Sends Second Look emails if commits so request
-- them.
postRootR :: Handler TypedContent
postRootR =
    -- TODO: Send (show request) in error email.
    -- Need to make YesodRequest an instance of Show.
    getRequest >>= \_ ->
    lookupPostParam "payload" >>=
    \case
        Just push -> handlePayload push >>
                    respond "text/plain" ("ok" :: T.Text)
        Nothing   -> liftIO (sendErrorEmail "Bad Request") >>
                    invalidArgs ["Expected parameter 'payload'"]

-- | handlePayload @push@ handles @push@ as if it were from GitHub, per the
-- specification in https://help.github.com/articles/post-receive-hooks#the-payload
handlePayload :: T.Text ->  -- | The POSTed data.
                Handler ()
handlePayload push =
    liftIO (putStrLn $ T.unpack push) >>
    decodePayload push >>=
    liftIO . sendSecondLookEmails

-- | decodePayload @push@ attempts to decode @push@ into a 'Payload'. If
-- unsuccessful, fires off an email to myself and returns a 400 invalid
-- arguments page.
decodePayload :: T.Text ->         -- The payload to decode.
                Handler Payload  -- The decoded payload.
decodePayload push =
    case eitherDecode (tl2bsl push) of
        Right payload -> return payload
        Left  err     -> do
            liftIO $ sendErrorEmail $ TL.unlines [ TL.pack err, (ts2tl push) ]
            invalidArgs [ "Invalid payload"
                        , "Aeson parse error: " `T.append` T.pack err
                        ]

-- | Sends Second Look emails (if necessary) with the commits in @payload@.
sendSecondLookEmails :: Payload  -- | The received payload (a single push).
                     -> IO ()
sendSecondLookEmails payload = do
    mapM_ (sendSecondLookEmail $ payload ^. pRepository) $ payload ^. pCommits

-- | sendSecondLookEmail @repo@ @commit@ sends a Second Look email if @commit@
-- contains the string "@username". Uses fields in @repo@ and @commit@ to
-- populate the email message.
sendSecondLookEmail :: PayloadRepository  -- | The repository pushed to.
                    -> PayloadCommit      -- | One commit of the push.
                    -> IO ()
sendSecondLookEmail repo commit = do
    let username = bs2ts $ BS.drop 1 $ -- Drop the '@'
            (commit ^. pcMessage) =~ ("@\\w+" :: BS.ByteString)
    unless (T.null username) $
        userInfoFor (T.unpack username) >>=
        either (const $ return ())
               (\user_info ->
                   doSendEmail username (T.pack $ detailedOwnerEmail user_info))
  where
    doSendEmail :: T.Text -> T.Text -> IO ()
    doSendEmail username user_email =
        bsl2tl <$> readTemplate "templates/plain.mustache" >>= \plain_body ->
        bsl2tl <$> readTemplate "templates/html.mustache"  >>= \html_body ->
        simpleMail from to subject plain_body html_body attachments >>=
        sendEmail
      where
        readTemplate :: String -> IO BSL.ByteString
        readTemplate file_path = hastacheFile defaultConfig
                                              file_path
                                              (mkStrContext context)
        context "commit_author"    = MuVariable $ commit ^. pcAuthor ^. puName
        context "commit_id"        = MuVariable $ commit ^. pcId
        context "commit_message"   = MuVariable $ commit ^. pcMessage
        context "commit_timestamp" = MuVariable $ commit ^. pcTimestamp
        context "commit_url"       = MuVariable $ commit ^. pcUrl
        context "repo_url"         = MuVariable $ repo   ^. prUrl
        context "repo_name"        = MuVariable $ repo   ^. prName
        context "username"         = MuVariable $ username

        from        = Address (Just "SecondLook") "mitchellwrosen@gmail.com"
        to          = Address Nothing             user_email
        subject     = username `T.append` " is requesting code review via Second Look"
        attachments = []

-- | Perform simple sanity checks to make sure the server will run properly.
-- This basically just calls functions that will error.
sanityChecks :: IO ()
sanityChecks = do
    -- Make sure keys are present in environment variables.
    access_key <- getEnv "SECOND_LOOK_ACCESS_KEY"
    secret_key <- getEnv "SECOND_LOOK_SECRET_KEY"
    putStrLn $ "Access key = " ++ access_key
    putStrLn $ "Secret key = " ++ secret_key

    -- Make sure email templates exist
    plain_perms <- getPermissions "templates/plain.mustache"
    unless (readable plain_perms) $ error "templates/plain.mustache unreadable"

    html_perms <- getPermissions "templates/html.mustache"
    unless (readable html_perms) $ error "templates/html.mustache unreadable"

-- | The main entry point of the program. Performs sanity checks to make sure
-- various environment variables and files exist, then launches the web server.
main :: IO ()
main = sanityChecks >> warp 8080 SecondLook
