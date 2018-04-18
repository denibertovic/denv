{-# LANGUAGE OverloadedStrings #-}

module Denv.Types where

import qualified Data.Text as T


type MakefileTemplateName = String
type KubeProjectName = String
type KubeNamespace = String
type PasswordStorePath = String

data Shell = BASH | ZSH deriving (Show, Eq, Read)

data EnvironmentType = Prod | Staging | Other String deriving (Eq, Read)

instance Show EnvironmentType where
    show Prod      = "prod"
    show Staging   = "staging"
    show (Other s) = s

data DenvVariable = KubeConfig
                  | KubeConfigShort
                  | PasswordStoreDir
                  | PasswordStoreDirShort
                  | Prompt
                  | OldPrompt
                  | KubectlNamespace deriving (Eq)

instance Show DenvVariable where
    show KubeConfig            = "KUBECONFIG"
    show KubeConfigShort       = "KUBECONFIG_SHORT"
    show PasswordStoreDir      = "PASSWORD_STORE_DIR"
    show PasswordStoreDirShort = "PASSWORD_STORE_DIR_SHORT"
    show Prompt                = "PS1"
    show OldPrompt             = "_OLD_DENV_PS1"
    show KubectlNamespace      = "KUBECTL_NAMESPACE"

data EnvVar = EnvVar DenvVariable String | Unset DenvVariable deriving (Eq, Show)

envify :: EnvVar -> T.Text
envify (EnvVar k v)                  = set (show k) v
envify (Unset KubeConfig)            = unset $ show KubeConfig
envify (Unset KubeConfigShort)       = unset $ show KubeConfigShort
envify (Unset PasswordStoreDir)      = unset $ show PasswordStoreDir
envify (Unset PasswordStoreDirShort) = unset $ show PasswordStoreDirShort
envify (Unset Prompt)                = unset "_FAKE_DENV_PS1" -- We don't ever wanna unset PS1
envify (Unset OldPrompt)             = unset $ show OldPrompt
envify (Unset KubectlNamespace)      = unset $ show KubectlNamespace

unset k = T.pack $ "unset " ++ k ++ ";" ++ "\n"
set k v = T.pack $ "export " ++ k ++ "=" ++ v ++ ";" ++ "\n"

toEnv :: [EnvVar] -> T.Text
toEnv xs = T.intercalate "" (map envify xs)

