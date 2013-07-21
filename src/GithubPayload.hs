{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GithubPayload where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import Data.Aeson
    ( FromJSON
    , Value(..)
    , (.:), (.:?), parseJSON
    )
import Data.ByteString (ByteString)
import Data.Text (Text)

data PayloadUser = PayloadUser
    { _userName     :: Text
    , _userEmail    :: Maybe Text
    , _userUsername :: Maybe Text
    } deriving (Show)
makeLenses ''PayloadUser

data PayloadCommit = PayloadCommit
    { _commitAdded     :: [Text]
    , _commitAuthor    :: PayloadUser
    , _commitCommitter :: PayloadUser
    , _commitDistinct  :: Bool
    , _commitId        :: Text
    , _commitMessage   :: ByteString -- Not Text, because ByteString is RegexLike
    , _commitModified  :: [Text]
    , _commitRemoved   :: [Text]
    , _commitTimestamp :: Text
    , _commitUrl       :: Text
    } deriving (Show)
makeLenses ''PayloadCommit

data PayloadRepository = PayloadRepository
    { _repoCreatedAt    :: Integer
    , _repoDescription  :: Text
    , _repoFork         :: Bool
    , _repoForks        :: Integer
    , _repoHasDownloads :: Bool
    , _repoHasIssues    :: Bool
    , _repoHasWiki      :: Bool
    , _repoId           :: Integer
    , _repoMasterBranch :: Text
    , _repoName         :: Text
    , _repoOpenIssues   :: Integer
    , _repoOwner        :: PayloadUser
    , _repoPrivate      :: Bool
    , _repoPushedAt     :: Integer
    , _repoSize         :: Integer
    , _repoStargazers   :: Integer
    , _repoUrl          :: Text
    , _repoWatchers     :: Integer
    } deriving (Show)
makeLenses ''PayloadRepository

data Payload = Payload
    { _payloadAfter      :: Text
    , _payloadBefore     :: Text
    , _payloadCommits    :: [PayloadCommit]
    , _payloadCompare    :: Text
    , _payloadCreated    :: Bool
    , _payloadDeleted    :: Bool
    , _payloadForced     :: Bool
    , _payloadHeadCommit :: PayloadCommit
    , _payloadPusher     :: PayloadUser
    , _payloadRef        :: Text
    , _payloadRepository :: PayloadRepository
    } deriving (Show)
makeLenses ''Payload

--------------------------------------------------------------------------------

instance FromJSON PayloadUser where
    parseJSON (Object o) =
        PayloadUser      <$>
        o .:  "name"     <*>
        o .:? "email"    <*>
        o .:? "username"
    parseJSON _ = mzero

instance FromJSON PayloadCommit where
    parseJSON (Object o) =
        PayloadCommit    <$>
        o .: "added"     <*>
        o .: "author"    <*>
        o .: "committer" <*>
        o .: "distinct"  <*>
        o .: "id"        <*>
        o .: "message"   <*>
        o .: "modified"  <*>
        o .: "removed"   <*>
        o .: "timestamp" <*>
        o .: "url"
    parseJSON _ = mzero

instance FromJSON PayloadRepository where
    parseJSON (Object o) =
        PayloadRepository    <$>
        o .: "created_at"    <*>
        o .: "description"   <*>
        o .: "fork"          <*>
        o .: "forks"         <*>
        o .: "has_downloads" <*>
        o .: "has_issues"    <*>
        o .: "has_wiki"      <*>
        o .: "id"            <*>
        o .: "master_branch" <*>
        o .: "name"          <*>
        o .: "open_issues"   <*>
        o .: "owner"         <*>
        o .: "private"       <*>
        o .: "pushed_at"     <*>
        o .: "size"          <*>
        o .: "stargazers"    <*>
        o .: "url"           <*>
        o .: "watchers"
    parseJSON _ = mzero

instance FromJSON Payload where
    parseJSON (Object o) =
        Payload            <$>
        o .: "after"       <*>
        o .: "before"      <*>
        o .: "commits"     <*>
        o .: "compare"     <*>
        o .: "created"     <*>
        o .: "deleted"     <*>
        o .: "forced"      <*>
        o .: "head_commit" <*>
        o .: "pusher"      <*>
        o .: "ref"         <*>
        o .: "repository"
    parseJSON _ = mzero
