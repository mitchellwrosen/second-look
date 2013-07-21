{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (unless)
import System.Environment (getArgs, getEnv)
import System.Directory (getPermissions, readable)
import Yesod.Core.Dispatch (warp)

import SecondLook (SecondLook(..))

main :: IO ()
main =
    performSanityChecks >>
    getArgs >>=
    \case
        ("--debug":_) -> warp 8080 SecondLook
        _             -> warp 80   SecondLook

-- Perform simple sanity checks to make sure the server will run properly. This basically just calls functions that will
-- error.
performSanityChecks :: IO ()
performSanityChecks =
    sanityCheckEnvironmentVariables >>
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
