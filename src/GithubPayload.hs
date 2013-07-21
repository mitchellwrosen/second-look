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
    { _puName     :: Text
    , _puEmail    :: Maybe Text
    , _puUsername :: Maybe Text
    } deriving (Show)
makeLenses ''PayloadUser

data PayloadCommit = PayloadCommit
    { _pcAdded     :: [Text]
    , _pcAuthor    :: PayloadUser
    , _pcCommitter :: PayloadUser
    , _pcDistinct  :: Bool
    , _pcId        :: Text
    , _pcMessage   :: ByteString -- Not Text, because ByteString is RegexLike
    , _pcModified  :: [Text]
    , _pcRemoved   :: [Text]
    , _pcTimestamp :: Text
    , _pcUrl       :: Text
    } deriving (Show)
makeLenses ''PayloadCommit

data PayloadRepository = PayloadRepository
    { _prCreatedAt    :: Integer
    , _prDescription  :: Text
    , _prFork         :: Bool
    , _prForks        :: Integer
    , _prHasDownloads :: Bool
    , _prHasIssues    :: Bool
    , _prHasWiki      :: Bool
    , _prId           :: Integer
    , _prMasterBranch :: Text
    , _prName         :: Text
    , _prOpenIssues   :: Integer
    , _prOwner        :: PayloadUser
    , _prPrivate      :: Bool
    , _prPushedAt     :: Integer
    , _prSize         :: Integer
    , _prStargazers   :: Integer
    , _prUrl          :: Text
    , _prWatchers     :: Integer
    } deriving (Show)
makeLenses ''PayloadRepository

data Payload = Payload
    { _pAfter      :: Text
    , _pBefore     :: Text
    , _pCommits    :: [PayloadCommit]
    , _pCompare    :: Text
    , _pCreated    :: Bool
    , _pDeleted    :: Bool
    , _pForced     :: Bool
    , _pHeadCommit :: PayloadCommit
    , _pPusher     :: PayloadUser
    , _pRef        :: Text
    , _pRepository :: PayloadRepository
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
