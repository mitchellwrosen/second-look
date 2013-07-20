{-# LANGUAGE OverloadedStrings #-}

module GithubPush where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
    ( FromJSON
    , Value(..)
    , (.:), (.:?), parseJSON
    )

data GithubPush = GithubPush
    { pushAfter      :: String
    , pushBefore     :: String
    , pushCommits    :: [Commit]
    , pushCompare    :: String
    , pushCreated    :: Bool
    , pushDeleted    :: Bool
    , pushForced     :: Bool
    , pushHeadCommit :: Commit
    , pushPusher     :: Person
    , pushRef        :: String
    , pushRepository :: Repository
    } deriving (Show)

instance FromJSON GithubPush where
    parseJSON (Object o) =
        GithubPush         <$>
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

data Commit = Commit
    { commitAdded     :: [String]
    , commitAuthor    :: Person
    , commitCommitter :: Person
    , commitDistinct  :: Bool
    , commitId        :: String
    , commitMessage   :: String
    , commitModified  :: [String]
    , commitRemoved   :: [String]
    , commitTimestamp :: String
    , commitUrl       :: String
    } deriving (Show)

instance FromJSON Commit where
    parseJSON (Object o) =
        Commit           <$>
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

data Person = Person
    { personName     :: String
    , personEmail    :: String
    , personUsername :: Maybe String
    } deriving (Show)

instance FromJSON Person where
    parseJSON (Object o) =
        Person           <$>
        o .:  "name"     <*>
        o .:  "email"    <*>
        o .:? "username"
    parseJSON _ = mzero

data Repository = Repository
    { repositoryCreatedAt    :: Integer
    , repositoryDescription  :: String
    , repositoryFork         :: Bool
    , repositoryForks        :: Integer
    , repositoryHasDownloads :: Bool
    , repositoryHasIssues    :: Bool
    , repositoryHasWiki      :: Bool
    , repositoryId           :: Integer
    , repositoryMasterBranch :: String
    , repositoryName         :: String
    , repositoryOpenIssues   :: Integer
    , repositoryOwner        :: Person
    , repositoryPrivate      :: Bool
    , repositoryPushedAt     :: Integer
    , repositorySize         :: Integer
    , repositoryStargazers   :: Integer
    , repositoryUrl          :: String
    , repositoryWatchers     :: Integer
    } deriving (Show)

instance FromJSON Repository where
    parseJSON (Object o) =
        Repository           <$>
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
