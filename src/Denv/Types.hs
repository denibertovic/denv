{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Denv.Types where

import RIO hiding (set)

import qualified Data.Text as T
import Data.Typeable

type KubeProjectName = String

type KubeNamespace = String
type TillerNamespace = String

type PasswordStorePath = String
type GoogleCredentialsPath = String

data Shell
  = BASH
  | ZSH
  deriving (Show, Eq, Read)

data GcpVariable
 = GoogleCredentials
 | GoogleCredentialsShort
 deriving (Eq)

instance Show GcpVariable where
  show GoogleCredentials = "GOOGLE_CREDENTIALS"
  show GoogleCredentialsShort = "GOOGLE_CREDENTIALS_SHORT"

data AwsVariable
  = AwsAccessKeyId
  | AwsSecretAccessKey
  | AwsSessionToken
  | AwsSecurityToken
  | AwsDefaultRegion
  | AwsRegion
  deriving (Eq)

instance Show AwsVariable where
  show AwsAccessKeyId = "AWS_ACCESS_KEY_ID"
  show AwsSecretAccessKey = "AWS_SECRET_ACCESS_KEY"
  show AwsSessionToken = "AWS_SESSION_TOKEN"
  show AwsSecurityToken = "AWS_SECURITY_TOKEN"
  show AwsDefaultRegion = "AWS_DEFAULT_REGION"
  show AwsRegion = "AWS_REGION"

data KubeVariable
  = KubeConfig
  | KubeConfigShort
  | KubectlNamespace
  | TillerNamespace
  deriving (Eq)

instance Show KubeVariable where
  show KubeConfig = "KUBECONFIG"
  show KubeConfigShort = "KUBECONFIG_SHORT"
  show KubectlNamespace = "KUBECTL_NAMESPACE"
  show TillerNamespace = "TILLER_NAMESPACE"

data PassVariable
  = PasswordStoreDir
  | PasswordStoreDirShort
  deriving (Eq)

instance Show PassVariable where
  show PasswordStoreDir = "PASSWORD_STORE_DIR"
  show PasswordStoreDirShort = "PASSWORD_STORE_DIR_SHORT"

data SpecialVariable
  = Prompt
  | OldPrompt
  | DenvSetVars
  | RawEnvFile
  deriving (Eq)

instance Show SpecialVariable where
  show Prompt = "PS1"
  show OldPrompt = "_OLD_DENV_PS1"
  show DenvSetVars = "_DENV_SET_VARS"
  show RawEnvFile = "RAW_ENV_FILE"

data DenvVariable where
  Set :: (Eq a, Show a, Typeable a) => a -> T.Text -> DenvVariable
  Unset :: (Eq a, Show a, Typeable a) => a -> DenvVariable

instance Eq DenvVariable where
  Set k1 v1 == Set k2 v2 = Just k1 == cast k2 && v1 == v2
  Unset x == Unset y     = Just x == cast y
  _ == _                 = False
