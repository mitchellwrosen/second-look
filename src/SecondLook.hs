{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import System.Environment (getEnv)

import Control.Monad.Trans (liftIO)
import Control.Lens ((^.))
import Data.Aeson (eitherDecode)
import Data.Data (Data)
import Data.Generics (Typeable)
import Github.Users (DetailedOwner, detailedOwnerEmail, detailedOwnerName, userInfoFor)
import Network.Mail.Mime (Address(..), Mail, simpleMail)
import System.Directory (getPermissions, readable)
import System.Environment (getArgs)
import Text.Hastache (defaultConfig, hastacheFile)
import Text.Hastache.Context (mkGenericContext)
import Text.Regex.PCRE ((=~))
import Yesod.Core (Yesod)
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Dispatch (mkYesod, parseRoutes, warp)
import Yesod.Core.Handler (getRequest, invalidArgs, lookupPostParam, respond)
import Yesod.Routes.Class (renderRoute)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

import Email (sendEmail, sendErrorEmail)
import GithubPayload
import Text.Extras (bsl2tl, tl2bsl, ts2tl)

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

--------------------------------------------------------------------------------

data SecondLookEmail = SecondLookEmail
    { sleCommitAuthor    :: !T.Text
    , sleCommitId        :: !T.Text
    , sleCommitMessage   :: !BS.ByteString
    , sleCommitTimestamp :: !T.Text
    , sleCommitUrl       :: !T.Text
    , sleRepoUrl         :: !T.Text
    , sleRepoName        :: !T.Text
    , sleUsername        :: !T.Text
    } deriving (Data, Typeable)

-- | Regex for \username
githubUsernameRegex :: BS.ByteString
githubUsernameRegex = "\\\\\\w+"

-- | Regex for \user@domain.com
emailHandleRegex :: BS.ByteString
emailHandleRegex = "\\\\\\b[\\w\\d.%+-]+@[\\w\\d.-]+\\.[a-zA-Z]{2,4}+\\b"

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
    maybeSendEmailToGitHubUsername
    maybeSendEmailToEmailHandle
  where
    maybeSendEmailToEmailHandle = undefined

    -- | Searches the commit message for \username. If found, and the username
    -- is valid, sends an email. Otherwise, does nothing.
    maybeSendEmailToGitHubUsername :: IO ()
    maybeSendEmailToGitHubUsername =
        let
            commit_message  = commit ^. pcMessage
            github_username = BS.unpack (commit_message =~ githubUsernameRegex :: BS.ByteString)
        in
            userInfoFor github_username >>=
            either (const $ return ())
                   (sendEmailToGitHubUser github_username)

    -- | Sends an email to a GitHub user given @github_username@ and
    -- @github_user_info@.
    sendEmailToGitHubUser :: String -> DetailedOwner -> IO ()
    sendEmailToGitHubUser github_username github_user_info = do
        let full_name  = detailedOwnerName  github_user_info
            email_addr = detailedOwnerEmail github_user_info
        doSendEmail (T.pack     github_username)
                    (T.pack <$> full_name)
                    (T.pack email_addr)

    -- | Sends an email to a user, addressing him or her by username, but also
    -- includes their full name if available.
    doSendEmail :: T.Text       ->  -- | The username of the recipient.
                  Maybe T.Text ->  -- | The full name of the recipient.
                  T.Text       ->  -- | The email address of the recipient.
                  IO ()
    doSendEmail recipient_username recipient_full_name recipient_email = do
        plain_body <- readTemplate "templates/plain.mustache" second_look_email
        html_body  <- readTemplate "templates/html.mustache"  second_look_email
        createEmail plain_body html_body >>= sendEmail
      where
        commit_author = commit ^. pcAuthor ^. puName
        second_look_email = SecondLookEmail
            { sleCommitAuthor    = commit_author
            , sleCommitId        = commit ^. pcId
            , sleCommitMessage   = commit ^. pcMessage
            , sleCommitTimestamp = commit ^. pcTimestamp
            , sleCommitUrl       = commit ^. pcUrl
            , sleRepoUrl         = repo   ^. prUrl
            , sleRepoName        = repo   ^. prName
            , sleUsername        = recipient_username
            }

        createEmail :: TL.Text ->  -- | Plain body.
                      TL.Text ->  -- | HTML body.
                      IO Mail
        createEmail plain_body html_body =
            simpleMail to from subject plain_body html_body attachments

        to          = Address recipient_full_name recipient_email
        from        = Address (Just "Second Look") "mitchellwrosen@gmail.com"
        subject     = commit_author `T.append` " is requesting code review via Second Look"
        attachments = []

readTemplate :: String -> SecondLookEmail -> IO TL.Text
readTemplate file_path context = bsl2tl <$>
    hastacheFile defaultConfig file_path (mkGenericContext context)

-- | Perform simple sanity checks to make sure the server will run properly.
-- This basically just calls functions that will error.
sanityChecks :: IO ()
sanityChecks = do
    sanityCheckEnvironmentVariables
    sanityCheckEmailTemplates
  where
    sanityCheckEnvironmentVariables :: IO ()
    sanityCheckEnvironmentVariables = do
        access_key <- getEnv "SECOND_LOOK_ACCESS_KEY"
        secret_key <- getEnv "SECOND_LOOK_SECRET_KEY"
        putStrLn $ "Access key = " ++ access_key
        putStrLn $ "Secret key = " ++ secret_key

    sanityCheckEmailTemplates :: IO ()
    sanityCheckEmailTemplates = do
        errorIfCannotRead "templates/plain.mustache"
        errorIfCannotRead "templates/html.mustache"

    errorIfCannotRead :: FilePath -> IO ()
    errorIfCannotRead file_path = do
        plain_perms <- getPermissions file_path
        unless (readable plain_perms) $ error (file_path ++ " unreadable")

-- | The main entry point of the program. Performs sanity checks to make sure
-- various environment variables and files exist, then launches the web server.
-- Run with single argument "--debug" to bind to port 8080.
main :: IO ()
main =
    sanityChecks >>
    getArgs >>=
    \case
        ("--debug":_) -> warp 8080 SecondLook
        _             -> warp 80 SecondLook
