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

-- role cache credentials: we already made a STS request and assumed a role and got back a key/secret that expires
-- session cache credentials: these are the STS session credentials used for requesting the Role Creds. Must have MFA
-- no session creds: using just what's in the config file: here we just use the key and secret specified in the
--     config file
-- data AwsEnvF a =
--     CachedSTS AwsEnvSessionCache
--   | CachedRole AwsEnvRoleCache
--   | STS AWS.AccessKey AWS.SecretKey MfaSerial -- Note how we require MFA
--   | STSWithRole AWS.AccessKey AWS.SecretKey MfaSerial RoleArn  -- Note how we require MFA here
--   | Raw AWS.AccessKey AWS.SecretKey
--   | RawWithRole AWS.AccessKey AWS.SecretKey RoleArn
--   deriving (Functor)

-- type AwsEnv = Free AwsEnvF

newtype AwsEnvT a =  AwsEnvT { runAwsEnvT :: ReaderT AwsEnvConfig IO a}

-- liftF :: Functor f => f a -> Free f a
-- liftF command = Free (fmap Pure command)

-- runAwsEnv :: AwsEnv a -> IO ()
-- runAwsEnv (Free (CachedSTS sessionCache)) = undefined
-- runAwsEnv (Free (CachedRole roleCache)) = undefined
-- runAwsEnv (Free (STS key secret mfa)) = undefined
-- runAwsEnv (Free (STSWithRole key secret mfa role)) = undefined
-- runAwsEnv (Free (Raw key secret)) = undefined
-- runAwsEnv (Free (RawWithRole key secret role)) = undefined

-- data AwsEnvAuth a where
--   CachedSTS :: AwsEnvSessionCache ->
--   CachedRole :: AwsEnvRoleCache -> _

--   STS :: AWS.AccessKey -> AWS.SecretKey -> MfaSerial -> _
--   STSWithRole :: AWS.AccessKey -> AWS.SecretKey -> MfaSerial -> RoleArn -> _

--   Raw :: AWS.AccessKey -> AWS.SecretKey -> Maybe MfaSerial -> _
--   RawWithRole :: AWS.AccessKey -> AWS.SecretKey -> Maybe MfaSerial -> RoleArn -> _


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

-- Here's an annotated example (all fields are optional, and defaults or values
-- from the AWS CLI configuration will be used for any ommitted values):
--     # The default AWS region.
--     region=us-east-1
--     # Profile name. Note that this profile does not need to exist in the AWS CLI
--     # configuration if a source_profile is set, but it is displayed to the user
--     # and in the prompt.
--     profile=admin
--     # The AWS CLI profile to use when creating the temporary session.
--     source_profile=signin
--     # The ARN of the role to assume.
--     role_arn=arn:aws:iam::123456789000:role/admin
--     # How long until the MFA session expires. This defaults to the maximum
--     # 129600 (36 hours).
--     mfa_duration_seconds=43200
--     # How long until the role session expires. This defaults to the maximum 3600
--     # (1 hour).  AWS: From 1 hours to max 12 hours (set on a per role basis)
--     # so if role has it's max set to 5 minutes you can't exceed that
--     role_duration_seconds=1800
--     # Percentage of MFA token duration at which to refresh it. The default is to
--     # request a new token after 65% of the old token's duration has passed.
--     mfa_refresh_factor=50
--     # Percentage of role token duration at which to refresh it. The default is
--     # to request a new token after 35% of the old token's duration has passed.
--     role_refresh_factor=10
--     # The 'printf' format for the profile in the bash prompt. This can include
--     # terminal escape sequences to change the colour.  Defaults to '[%s]'
--     prompt_format=\[\033[1;31m\](%s)\[\033[0m\]
-- ### The following environment variables are read by aws-env:
-- `AWS_DEFAULT_PROFILE`: AWS CLI profile to use. Defaults to `default`.
-- `default` is used.
-- `AWS_CONFIG_FILE`: Location of the AWS CLI config file. Defaults to
-- `~/.aws/config`.
-- `AWS_ENV_CACHE_DIR`: Location of cached credentials. Defaults to
-- `~/.aws-env/`
-- ### The following standard AWS environment variables are **set** by aws-env:
-- - `AWS_ACCESS_KEY_ID`
-- - `AWS_SECRET_ACCESS_KEY`
-- - `AWS_SESSION_TOKEN`
-- - `AWS_SECURITY_TOKEN`
-- - `AWS_DEFAULT_REGION`
-- In addition, `AWS_ENV_CURRENT_PROFILE` is set to the name of the current
-- profile.
