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

data GHUser = GHUser
    { _userName     :: !Text
    , _userEmail    :: Maybe Text
    , _userUsername :: Maybe Text
    } deriving (Show)
makeLenses ''GHUser

data GHCommit = GHCommit
    { _commitAdded     :: [Text]
    , _commitAuthor    :: GHUser
    , _commitCommitter :: GHUser
    , _commitDistinct  :: Bool
    , _commitId        :: Text
    , _commitMessage   :: ByteString -- Not Text, because ByteString is RegexLike
    , _commitModified  :: [Text]
    , _commitRemoved   :: [Text]
    , _commitTimestamp :: Text
    , _commitUrl       :: Text
    } deriving (Show)
makeLenses ''GHCommit

data GHRepository = GHRepository
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
    , _repoOwner        :: GHUser
    , _repoPrivate      :: Bool
    , _repoPushedAt     :: Integer
    , _repoSize         :: Integer
    , _repoStargazers   :: Integer
    , _repoUrl          :: Text
    , _repoWatchers     :: Integer
    } deriving (Show)
makeLenses ''GHRepository

data GHPayload = GHPayload
    { _payloadAfter      :: Text
    , _payloadBefore     :: Text
    , _payloadCommits    :: [GHCommit]
    , _payloadCompare    :: Text
    , _payloadCreated    :: Bool
    , _payloadDeleted    :: Bool
    , _payloadForced     :: Bool
    , _payloadHeadCommit :: GHCommit
    , _payloadPusher     :: GHUser
    , _payloadRef        :: Text
    , _payloadRepository :: GHRepository
    } deriving (Show)
makeLenses ''GHPayload

--------------------------------------------------------------------------------

instance FromJSON GHUser where
    parseJSON (Object o) =
        GHUser           <$>
        o .:  "name"     <*>
        o .:? "email"    <*>
        o .:? "username"
    parseJSON _ = mzero

instance FromJSON GHCommit where
    parseJSON (Object o) =
        GHCommit         <$>
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

instance FromJSON GHRepository where
    parseJSON (Object o) =
        GHRepository         <$>
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

instance FromJSON GHPayload where
    parseJSON (Object o) =
        GHPayload          <$>
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
