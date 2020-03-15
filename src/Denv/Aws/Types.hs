{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}

module Denv.Aws.Types where

import RIO
import RIO.Process

import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Network.AWS as AWS
import qualified RIO.Text as T
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Free

import Network.AWS.Data.Time (UTCTime, ISO8601)

data AwsEnvAuth = AwsEnvAuth AWS.AccessKey AWS.SecretKey (Maybe AWS.SessionToken)

newtype AwsEnvT a =  AwsEnvT { runAwsEnvT :: ReaderT AwsEnvConfig IO a}

newtype MfaSerial = MfaSerial T.Text deriving (Eq, Show, Read)
newtype RoleArn = RoleArn T.Text deriving (Eq, Show, Read)

-- TODO: Remove AwsEnvCacheNoMfa and AwsEnvCacheNoRoleArn
data AwsEnvCacheError = AwsEnvCacheExpired | AwsEnvCacheParseError | AwsEnvCacheNotFound | AwsEnvCacheNoMfa | AwsEnvCacheNoRoleArn deriving (Eq, Show)

newtype AwsProfile =
  AwsProfile T.Text
  deriving (Eq, Show)

instance FromJSON AwsProfile where
  parseJSON (JSON.String s) = return $ AwsProfile s
  parseJSON _ = fail "Error parsing profile"

newtype AwsSourceProfile =
  AwsSourceProfile AwsProfile
  deriving (Eq, Show)

instance FromJSON AwsSourceProfile where
  parseJSON (JSON.String s) = return $ AwsSourceProfile $ AwsProfile s
  parseJSON _ = fail "Error parsing source profile"

fromAwsProfile :: AwsProfile -> T.Text
fromAwsProfile (AwsProfile p) = p

fromAwsSourceProfile :: AwsSourceProfile -> T.Text
fromAwsSourceProfile (AwsSourceProfile (AwsProfile p)) = p

fromAwsSourceProfileMaybe :: Maybe AwsSourceProfile -> T.Text
fromAwsSourceProfileMaybe sp = case sp of
  Nothing -> "nonexistentsourceprofile"
  Just (AwsSourceProfile (AwsProfile p)) -> p


mkP :: AwsProfile -> T.Text
mkP p = "profile " <> fromAwsProfile p

mkSP :: AwsSourceProfile -> T.Text
mkSP sp = "profile " <> fromAwsSourceProfile sp

mkSPMaybe :: Maybe AwsSourceProfile -> T.Text
mkSPMaybe sp =
  maybe
    "nonexistentsourceprofile"
    (\sp' -> "profile " <> fromAwsSourceProfile sp')
    sp

data AwsEnv = AwsEnv
  { -- awsEnvConfig :: !AwsEnvConfig
    awsEnvLogger :: !LogFunc
  , awsEnvProcessContext :: !ProcessContext
  }

instance HasProcessContext AwsEnv where
  processContextL = lens awsEnvProcessContext (\x y -> x {awsEnvProcessContext = y})

-- class HasConfig env where
--   configL :: Lens' env AwsEnvConfig

-- instance HasConfig AwsEnv where
--   configL = lens awsEnvConfig (\x y -> x {awsEnvConfig = y})

instance HasLogFunc AwsEnv where
  logFuncL = lens awsEnvLogger (\x y -> x {awsEnvLogger = y})

data AwsEnvConfig = AwsEnvConfig
  { awsEnvConfigRegion :: AWS.Region
  , awsEnvConfigProfile :: AwsProfile
  , awsEnvConfigSourceProfile :: AwsSourceProfile
  , awsEnvConfigRoleArn :: T.Text
  , awsEnvConfigMfaDurationSeconds :: Int
  , awsEnvConfigRoleDurationSeconds :: Int
  , awsEnvConfigMfaRefreshFactor :: Int
  , awsEnvConfigRoleRefreshFactor :: Int
  -- , awsEnvConfigPromptFormat :: T.Text -- This is not supported right now
  } deriving (Eq, Show)

instance FromJSON AwsEnvConfig where
  parseJSON (Object o) = do
    awsEnvConfigRegion <- o .: "region"
    awsEnvConfigProfile <- o .: "profile"
    awsEnvConfigSourceProfile <- o .: "source_profile"
    awsEnvConfigRoleArn <- o .: "role_arn"
    awsEnvConfigMfaDurationSeconds <- o .: "mfa_duration_seconds"
    awsEnvConfigRoleDurationSeconds <- o .: "role_duration_seconds"
    awsEnvConfigMfaRefreshFactor <- o .: "mfa_refresh_factor"
    awsEnvConfigRoleRefreshFactor <- o .: "role_refresh_factor"
    return $ AwsEnvConfig {..}
  parseJSON _ = fail "Expected Object for AwsEnvConfig value"

data AwsEnvRoleCache = AwsEnvRoleCache
  { roleCacheAccessKeyId :: AWS.AccessKey
  , roleCacheSecretAccessKey :: AWS.SecretKey
  , roleCacheSessionToken :: Maybe AWS.SessionToken
  , roleCacheExpiration :: Maybe UTCTime
  , roleCacheAssumedRoleUser :: Maybe AwsEnvAssumedRoleUser
  }

data AwsEnvAssumedRoleUser = AwsEnvAssumedRoleUser
  { awsEnvAssumedRoleUserId :: T.Text
  , awsEnvAssumedRoleUserArn :: T.Text
  } deriving (Eq, Show)

data AwsEnvSessionCache = AwsEnvSessionCache
  { sessionCacheAccessKeyId :: AWS.AccessKey
  , sessionCacheSecretAccessKey :: AWS.SecretKey
  , sessionCacheSessionToken :: Maybe AWS.SessionToken
  , sessionCacheExpiration :: Maybe UTCTime
  }

instance ToJSON AwsEnvAssumedRoleUser where
  toJSON c =
    object
      [ "AssumedRoleId" .= awsEnvAssumedRoleUserId c
      , "Arn" .= awsEnvAssumedRoleUserArn c
      ]

instance FromJSON AwsEnvAssumedRoleUser where
  parseJSON (Object o) = do
    i <- o .: "AssumedRoleId"
    a <- o .: "Arn"
    return $ AwsEnvAssumedRoleUser i a
  parseJSON _ = fail "AssumedRoleUser is not an Object."

instance ToJSON AwsEnvRoleCache where
  toJSON c =
    object
      [ "Credentials" .=
        object
          [ "AccessKeyId" .= roleCacheAccessKeyId c
          , "SecretAccessKey" .= roleCacheSecretAccessKey c
          , "SessionToken" .= roleCacheSessionToken c
          , "Expiration" .= roleCacheExpiration c
          ],
        "AssumedRoleUser" .= toJSON (roleCacheAssumedRoleUser c)
      ]

instance FromJSON AwsEnvRoleCache where
  parseJSON =
    withObject "Credentials" $ \v -> do
        c <- v .: "Credentials"
        AwsEnvRoleCache <$> c .: "AccessKeyId" <*> c .: "SecretAccessKey" <*>
          c .:? "SessionToken" <*>
          c .:? "Expiration" <*>
          v .:? "AssumedRoleUser"

instance ToJSON AwsEnvSessionCache where
  toJSON c =
    object
      [ "Credentials" .=
        object
          [ "AccessKeyId" .= sessionCacheAccessKeyId c
          , "SecretAccessKey" .= sessionCacheSecretAccessKey c
          , "SessionToken" .= sessionCacheSessionToken c
          , "Expiration" .= sessionCacheExpiration c
          ]
      ]

instance FromJSON AwsEnvSessionCache where
  parseJSON =
    withObject "Credentials" $ \v -> do
      c <- v .: "Credentials"
      AwsEnvSessionCache <$> c .: "AccessKeyId" <*> c .: "SecretAccessKey" <*>
        c .:? "SessionToken" <*>
        c .:? "Expiration"
