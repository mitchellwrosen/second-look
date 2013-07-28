{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module SecondLook where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Control.Lens ((^.))
import Data.Aeson (eitherDecode)
import Data.Data (Data)
import Data.Generics (Typeable)
import Github.Users (DetailedOwner, detailedOwnerEmail, detailedOwnerName, userInfoFor)
import Network.Mail.Mime (Address(..), Mail, simpleMail)
import Text.Hastache (defaultConfig, hastacheFile)
import Text.Hastache.Context (mkGenericContext)
import Text.Regex.PCRE (Regex, makeRegex)
import Text.Regex.Base.Extras (matchAllTextOnly)
import Yesod.Core (Yesod, makeSessionBackend)
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Dispatch (mkYesod, parseRoutes)
import Yesod.Core.Handler (getRequest, invalidArgs, lookupPostParam, respond)
import Yesod.Routes.Class (renderRoute)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

import Email (sendEmail, sendErrorEmail)
import GithubPayload
import Data.Text.Encoding.Extras (bs2t, bsl2tl, t2bsl, t2tl)

data SecondLook = SecondLook

mkYesod "SecondLook" [parseRoutes|
    / RootR POST
|]

instance Yesod SecondLook where
    makeSessionBackend _ = return Nothing

-- All of the information picked out of a payload and put into an email.
-- CHANGES HERE MUST BE REFLECTED IN templates/!!! There is no compile-time assurance that the {{selectors}} are
-- matching anything.
data SecondLookEmail = SecondLookEmail
    { sleCommitAuthor    :: !T.Text
    , sleCommitId        :: !T.Text
    , sleCommitMessage   :: !BS.ByteString -- ByteString because it's RegexLike and Text isn't.
    , sleCommitModified  :: ![T.Text]
    , sleCommitRemoved   :: ![T.Text]
    , sleCommitTimestamp :: !T.Text
    , sleCommitUrl       :: !T.Text
    , sleRepoCreatedAt   :: !Integer
    , sleRepoDescription :: !T.Text
    , sleRepoForks       :: !Integer
    , sleRepoName        :: !T.Text
    , sleRepoOpenIssues  :: !Integer
    , sleRepoSize        :: !Integer
    , sleRepoStargazers  :: !Integer
    , sleRepoWatchers    :: !Integer
    , sleRepoUrl         :: !T.Text
    , sleUsername        :: !T.Text
    } deriving (Data, Typeable)

-- \username
githubUsernameRegex :: Regex
githubUsernameRegex = makeRegex ("\\\\\\w+\\b" :: BS.ByteString)

-- \user@domain.com
emailHandleRegex :: Regex
emailHandleRegex = makeRegex ("\\\\[\\w\\d.%+-]+@[\\w\\d.-]+\\.[a-zA-Z]{2,4}+\\b" :: BS.ByteString)

postRootR :: Handler TypedContent
postRootR =
    getRequest >>= \_ -> -- TODO: Send (show request) in error email.
    lookupPostParam "payload" >>=
    \case
        Just push ->
            handlePayload push >>
            respond "text/plain" ("ok" :: T.Text)
        Nothing ->
            liftIO (sendErrorEmail ["Bad Request"]) >>
            invalidArgs ["Expected parameter 'payload'"]

handlePayload :: T.Text -> Handler ()
handlePayload push_json =
    decodePayload >>=
    liftIO . sendSecondLookEmails
  where
    decodePayload :: Handler Payload
    decodePayload =
        case eitherDecode (t2bsl push_json) of
            Right payload -> return payload
            Left  err     ->
                liftIO (sendErrorEmail [TL.pack err, t2tl push_json]) >>
                invalidArgs [push_json]

sendSecondLookEmails :: Payload -> IO ()
sendSecondLookEmails payload = do
    let repo    = payload ^. payloadRepository
        commits = payload ^. payloadCommits
    mapM_ (sendSecondLookEmail repo) commits

sendSecondLookEmail :: PayloadRepository -> PayloadCommit   -> IO ()
sendSecondLookEmail repo commit =
    maybeSendEmailToGitHubUsername >>
    maybeSendEmailToEmailHandle
  where
    maybeSendEmailToGitHubUsername :: IO ()
    maybeSendEmailToGitHubUsername =
        let
            commit_message   = commit ^. commitMessage
            github_usernames = T.drop 1 . bs2t <$> matchAllTextOnly githubUsernameRegex commit_message
        in
            forM_ github_usernames $ \github_username ->
                userInfoFor (T.unpack github_username) >>=
                \case
                    Right github_user_info -> sendEmailToGitHubUser github_username github_user_info
                    Left _ -> return () -- Matched \junk, just ignore

    sendEmailToGitHubUser :: T.Text -> DetailedOwner -> IO ()
    sendEmailToGitHubUser github_username github_user_info = do
        let full_name  = T.pack <$> detailedOwnerName  github_user_info
            email_addr = T.pack  $  detailedOwnerEmail github_user_info
        doSendEmail github_username full_name email_addr

    maybeSendEmailToEmailHandle :: IO ()
    maybeSendEmailToEmailHandle = do
        let commit_message = commit ^. commitMessage
            email_addrs    = T.drop 1 . bs2t <$> matchAllTextOnly emailHandleRegex commit_message
        forM_ email_addrs $ \email_addr ->
            doSendEmail email_addr (Just email_addr) email_addr

    doSendEmail :: T.Text -> Maybe T.Text -> T.Text -> IO ()
    doSendEmail recipient_handle recipient_full_name recipient_email = do
        plain_body <- readTemplate "templates/plain.mustache" second_look_email
        html_body  <- readTemplate "templates/html.mustache" second_look_email
        createEmail plain_body html_body >>= sendEmail
      where
        commit_author = commit ^. commitAuthor ^. userName

        second_look_email = SecondLookEmail
            { sleCommitAuthor    = commit_author
            , sleCommitId        = commit ^. commitId
            , sleCommitMessage   = commit ^. commitMessage
            , sleCommitModified  = commit ^. commitModified
            , sleCommitRemoved   = commit ^. commitRemoved
            , sleCommitTimestamp = commit ^. commitTimestamp
            , sleCommitUrl       = commit ^. commitUrl
            , sleRepoCreatedAt   = repo   ^. repoCreatedAt
            , sleRepoDescription = repo   ^. repoDescription
            , sleRepoForks       = repo   ^. repoForks
            , sleRepoOpenIssues  = repo   ^. repoOpenIssues
            , sleRepoSize        = repo   ^. repoSize
            , sleRepoStargazers  = repo   ^. repoStargazers
            , sleRepoWatchers    = repo   ^. repoWatchers
            , sleRepoName        = repo   ^. repoName
            , sleRepoUrl         = repo   ^. repoUrl
            , sleUsername        = recipient_handle
            }

        createEmail :: TL.Text -> TL.Text -> IO Mail
        createEmail plain_body html_body =
            simpleMail to from subject plain_body html_body attachments

        to          = Address recipient_full_name recipient_email
        from        = Address (Just "Second Look") "mitchellwrosen@gmail.com"
        subject     = commit_author `T.append` " is requesting code review"
        attachments = []

readTemplate :: String -> SecondLookEmail -> IO TL.Text
readTemplate file_path context = bsl2tl <$>
    hastacheFile defaultConfig file_path (mkGenericContext context)
