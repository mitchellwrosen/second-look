{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module SecondLook where

import Control.Applicative ((<$>))
import Control.Monad.Extras (mapMaybeM)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecode)
import Data.Data (Data)
import Data.Generics (Typeable)
import Data.Text (Text)
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

type GitHubUsername = Text
type EmailAddress   = Text

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
    { sleCommitAdded     :: ![Text]
    , sleCommitAuthor    :: !Text
    , sleCommitId        :: !Text
    , sleCommitMessage   :: !BS.ByteString -- ByteString because it's RegexLike and Text isn't.
    , sleCommitModified  :: ![Text]
    , sleCommitRemoved   :: ![Text]
    , sleCommitTimestamp :: !Text
    , sleCommitUrl       :: !Text
    , sleRepoCreatedAt   :: !Integer
    , sleRepoDescription :: !Text
    , sleRepoForks       :: !Integer
    , sleRepoName        :: !Text
    , sleRepoOpenIssues  :: !Integer
    , sleRepoSize        :: !Integer
    , sleRepoStargazers  :: !Integer
    , sleRepoWatchers    :: !Integer
    , sleRepoUrl         :: !Text
    , sleSalutation      :: !Text
    , sleFullName        :: !(Maybe Text)
    , sleEmailAddr       :: !Text
    } deriving (Data, Show, Typeable)

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
        Just payload ->
            handlePayload payload >>
            respond "text/plain" ("ok" :: Text)
        Nothing ->
            liftIO (sendErrorEmail ["Bad Request"]) >>
            invalidArgs ["Expected parameter 'payload'"]

handlePayload :: Text -> Handler ()
handlePayload payload =
    decodePayload payload >>=
    liftIO . payloadToEmails >>=
    liftIO . sendSecondLookEmails

decodePayload :: Text -> Handler GHPayload
decodePayload payload_json =
    case eitherDecode $ t2bsl payload_json of
        Right payload -> return payload
        Left  err     -> do
            liftIO $ sendErrorEmail [TL.pack err, t2tl payload_json]
            invalidArgs [payload_json]

payloadToEmails :: GHPayload -> IO [SecondLookEmail]
payloadToEmails payload = concat <$> mapM (makeEmails repo) commits
  where
    commits = payloadCommits    payload  -- :: [GHCommit]
    repo    = payloadRepository payload  -- :: GHRepository

makeEmails :: GHRepository -> GHCommit -> IO [SecondLookEmail]
makeEmails repo commit = do
    -- Unfortunate asymmetry: github emails require IO, regular emails don't
    github_emails <- makeEmailsToGitHubUsernames repo commit usernames
    let email_addr_emails = makeEmailsToEmailAddrs repo commit email_addrs
    return $ github_emails ++ email_addr_emails
  where
    commit_message = commitMessage commit
    usernames      = T.drop 1 . bs2t <$> matchAllTextOnly githubUsernameRegex commit_message
    email_addrs    = T.drop 1 . bs2t <$> matchAllTextOnly emailHandleRegex    commit_message

makeEmailsToGitHubUsernames :: GHRepository -> GHCommit -> [GitHubUsername] -> IO [SecondLookEmail]
makeEmailsToGitHubUsernames repo commit =
    mapMaybeM (makeEmailToGitHubUsername repo commit)

makeEmailToGitHubUsername :: GHRepository -> GHCommit -> GitHubUsername -> IO (Maybe SecondLookEmail)
makeEmailToGitHubUsername repo commit username =
    userInfoFor (T.unpack username) >>=
    \case
        Left  _         -> return Nothing -- Matched \junk, just ignore
        Right user_info -> return $ Just $ makeSecondLookEmail repo commit username full_name email_addr
          where
            full_name  = T.pack <$> detailedOwnerName  user_info
            email_addr = T.pack  $  detailedOwnerEmail user_info

makeEmailsToEmailAddrs :: GHRepository -> GHCommit -> [EmailAddress] -> [SecondLookEmail]
makeEmailsToEmailAddrs repo commit = map (makeEmailToEmailAddr repo commit)

makeEmailToEmailAddr :: GHRepository -> GHCommit -> EmailAddress -> SecondLookEmail
makeEmailToEmailAddr repo commit email_addr = makeSecondLookEmail repo commit email_addr Nothing email_addr

makeSecondLookEmail :: GHRepository -> GHCommit -> Text -> Maybe Text -> Text -> SecondLookEmail
makeSecondLookEmail repo commit salutation full_name email_addr = SecondLookEmail
    { sleCommitAdded     = commitAdded     commit
    , sleCommitAuthor    = userName      $ commitAuthor commit
    , sleCommitId        = commitId        commit
    , sleCommitMessage   = commitMessage   commit
    , sleCommitModified  = commitModified  commit
    , sleCommitRemoved   = commitRemoved   commit
    , sleCommitTimestamp = commitTimestamp commit
    , sleCommitUrl       = commitUrl       commit
    , sleRepoCreatedAt   = repoCreatedAt   repo
    , sleRepoDescription = repoDescription repo
    , sleRepoForks       = repoForks       repo
    , sleRepoOpenIssues  = repoOpenIssues  repo
    , sleRepoSize        = repoSize        repo
    , sleRepoStargazers  = repoStargazers  repo
    , sleRepoWatchers    = repoWatchers    repo
    , sleRepoName        = repoName        repo
    , sleRepoUrl         = repoUrl         repo
    , sleSalutation      = salutation
    , sleFullName        = full_name
    , sleEmailAddr       = email_addr
    }

sendSecondLookEmails :: [SecondLookEmail] -> IO ()
sendSecondLookEmails = mapM_ sendSecondLookEmail

sendSecondLookEmail :: SecondLookEmail -> IO ()
sendSecondLookEmail second_look_email = do
    plain_body <- readTemplate "templates/plain.mustache" second_look_email
    html_body  <- readTemplate "templates/html.mustache"  second_look_email
    createEmail plain_body html_body >>= sendEmail
      where
        createEmail :: TL.Text -> TL.Text -> IO Mail
        createEmail plain_body html_body =
            simpleMail to from subject plain_body html_body attachments

        to          = Address (sleFullName second_look_email) (sleEmailAddr second_look_email)
        from        = Address (Just "Second Look") "mitchellwrosen@gmail.com"
        subject     = (sleCommitAuthor second_look_email) `T.append` " is requesting code review"
        attachments = []

readTemplate :: String -> SecondLookEmail -> IO TL.Text
readTemplate file_path context = bsl2tl <$>
    hastacheFile defaultConfig file_path (mkGenericContext context)
