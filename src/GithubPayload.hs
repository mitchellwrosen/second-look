{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GithubPayload where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
    ( FromJSON
    , Value(..)
    , (.:), (.:?), parseJSON
    )
import Data.ByteString (ByteString)
import Data.Text (Text)

data GHUser = GHUser
    { userName     :: !Text
    , userEmail    :: Maybe Text
    , userUsername :: Maybe Text
    } deriving (Show)

data GHCommit = GHCommit
    { commitAdded     :: [Text]
    , commitAuthor    :: GHUser
    , commitCommitter :: GHUser
    , commitDistinct  :: Bool
    , commitId        :: Text
    , commitMessage   :: ByteString -- Not Text, because ByteString is RegexLike
    , commitModified  :: [Text]
    , commitRemoved   :: [Text]
    , commitTimestamp :: Text
    , commitUrl       :: Text
    } deriving (Show)

data GHRepository = GHRepository
    { repoCreatedAt    :: Integer
    , repoDescription  :: Text
    , repoFork         :: Bool
    , repoForks        :: Integer
    , repoHasDownloads :: Bool
    , repoHasIssues    :: Bool
    , repoHasWiki      :: Bool
    , repoId           :: Integer
    , repoMasterBranch :: Text
    , repoName         :: Text
    , repoOpenIssues   :: Integer
    , repoOwner        :: GHUser
    , repoPrivate      :: Bool
    , repoPushedAt     :: Integer
    , repoSize         :: Integer
    , repoStargazers   :: Integer
    , repoUrl          :: Text
    , repoWatchers     :: Integer
    } deriving (Show)

data GHPayload = GHPayload
    { payloadAfter      :: Text
    , payloadBefore     :: Text
    , payloadCommits    :: [GHCommit]
    , payloadCompare    :: Text
    , payloadCreated    :: Bool
    , payloadDeleted    :: Bool
    , payloadForced     :: Bool
    , payloadHeadCommit :: GHCommit
    , payloadPusher     :: GHUser
    , payloadRef        :: Text
    , payloadRepository :: GHRepository
    } deriving (Show)

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
