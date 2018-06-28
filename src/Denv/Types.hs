{-# LANGUAGE OverloadedStrings #-}

module Denv.Types where

import RIO hiding (set)

import qualified Data.Text as T

type MakefileTemplateName = String

type KubeProjectName = String

type KubeNamespace = String

type PasswordStorePath = String

data Shell
  = BASH
  | ZSH
  deriving (Show, Eq, Read)

data EnvironmentType
  = Prod
  | Staging
  | Other String
  deriving (Eq, Read)

instance Show EnvironmentType where
  show Prod = "prod"
  show Staging = "staging"
  show (Other s) = s

data DenvVariable
  = KubeConfig
  | KubeConfigShort
  | PasswordStoreDir
  | KubectlNamespace
  | PasswordStoreDirShort
  | VaultConfig
  | VaultAddr
  | VaultToken
  | VaultSkipVerify
  | Prompt
  | OldPrompt
  deriving (Eq)

instance Show DenvVariable where
  show KubeConfig = "KUBECONFIG"
  show KubeConfigShort = "KUBECONFIG_SHORT"
  show KubectlNamespace = "KUBECTL_NAMESPACE"
  show PasswordStoreDir = "PASSWORD_STORE_DIR"
  show PasswordStoreDirShort = "PASSWORD_STORE_DIR_SHORT"
  show VaultConfig = "VAULT_CONFIG"
  show VaultAddr = "VAULT_ADDR"
  show VaultToken = "VAULT_TOKEN"
  show VaultSkipVerify = "VAULT_SKIP_VERIFY"
  show Prompt = "PS1"
  show OldPrompt = "_OLD_DENV_PS1"

data EnvVar
  = EnvVar DenvVariable
           String
  | Unset DenvVariable
  deriving (Eq, Show)

envify :: EnvVar -> T.Text
envify (EnvVar k v) = set (show k) v
envify (Unset KubeConfig) = unset $ show KubeConfig
envify (Unset KubeConfigShort) = unset $ show KubeConfigShort
envify (Unset PasswordStoreDir) = unset $ show PasswordStoreDir
envify (Unset PasswordStoreDirShort) = unset $ show PasswordStoreDirShort
envify (Unset VaultConfig) = unset $ show VaultConfig
envify (Unset VaultAddr) = unset $ show VaultAddr
envify (Unset VaultToken) = unset $ show VaultToken
envify (Unset VaultSkipVerify) = unset $ show VaultSkipVerify
envify (Unset Prompt) = unset "_FAKE_DENV_PS1" -- We don't ever wanna unset PS1
envify (Unset OldPrompt) = unset $ show OldPrompt
envify (Unset KubectlNamespace) = unset $ show KubectlNamespace

unset k = T.pack $ "unset " ++ k ++ ";" ++ "\n"

set k v = T.pack $ "export " ++ k ++ "=" ++ v ++ ";" ++ "\n"

toEnv :: [EnvVar] -> T.Text
toEnv xs = T.intercalate "" (map envify xs)
