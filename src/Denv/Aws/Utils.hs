{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Denv.Aws.Utils where

import RIO

import qualified Data.Text as T
import qualified Network.AWS as AWS
import qualified Data.Text.Encoding as TE
import qualified Network.AWS as AWS
import qualified Network.AWS.STS.Types as STS
import System.FilePath ((</>))
import System.Directory (createDirectory, doesDirectoryExist, getHomeDirectory)

import Denv.Aws.Types

mkAwsSessionFileName :: AwsProfile -> Maybe AwsSourceProfile -> MfaSerial -> IO FilePath
mkAwsSessionFileName p sourceProfile (MfaSerial mfaSerial) =  do
  cacheDir <- makeCacheDir
  let profile = maybe (fromAwsProfile p) fromAwsSourceProfile sourceProfile
  return $ cacheDir </> (T.unpack $ profile <> "." <> normalize mfaSerial <> ".session_credentials.json")

mkAwsRoleFileName :: AwsProfile -> RoleArn -> IO FilePath
mkAwsRoleFileName p (RoleArn roleArn) = do
  cacheDir <- makeCacheDir
  return $ cacheDir </> (T.unpack $ (fromAwsProfile p) <> "." <> normalize roleArn <> ".role_credentials.json")

makeCacheDir :: IO FilePath
makeCacheDir = do
  h <- getHomeDirectory
  let dir = h </> ".aws-env"
  exists <- doesDirectoryExist dir
  unless exists (createDirectory dir)
  return dir

normalize :: T.Text -> T.Text
normalize t = T.replace "/" "_" $ T.replace ":" "_" t

mkAwsAccessKey :: T.Text -> AWS.AccessKey
mkAwsAccessKey t = AWS.AccessKey . TE.encodeUtf8 $ t

mkAwsSecretKey :: T.Text -> AWS.SecretKey
mkAwsSecretKey t = AWS.SecretKey . TE.encodeUtf8 $ t

toRoleCache :: AWS.AuthEnv -> STS.AssumedRoleUser -> AwsEnvRoleCache
toRoleCache creds ars =
  AwsEnvRoleCache
    { roleCacheAccessKeyId = creds ^. AWS.accessKeyId
    , roleCacheSecretAccessKey = creds ^. AWS.secretAccessKey
    , roleCacheSessionToken = creds ^. AWS.sessionToken
    , roleCacheExpiration = creds ^. AWS.expiration
    , roleCacheAssumedRoleUser =
        Just $
        AwsEnvAssumedRoleUser
          { awsEnvAssumedRoleUserId = ars ^. STS.aruAssumedRoleId
          , awsEnvAssumedRoleUserArn = ars ^. STS.aruARN
          }
    }

toSessionCache :: AWS.AuthEnv -> AwsEnvSessionCache
toSessionCache creds =  AwsEnvSessionCache
    { sessionCacheAccessKeyId = creds ^. AWS.accessKeyId
    , sessionCacheSecretAccessKey = creds ^. AWS.secretAccessKey
    , sessionCacheSessionToken = creds ^. AWS.sessionToken
    , sessionCacheExpiration = creds ^. AWS.expiration
    }
