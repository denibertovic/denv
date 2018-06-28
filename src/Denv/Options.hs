{-# LANGUAGE OverloadedStrings #-}

module Denv.Options where

import RIO

import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Version (showVersion)
import Denv.Types
import Options.Applicative
import Paths_denv (version)

data DenvArgs = DenvArgs
  { denvCommand :: Command
  }

data Command
  = Kube KubeProjectName
         (Maybe KubeNamespace)
  | Pass (Maybe PasswordStorePath)
  | Terraform EnvironmentType
  | Fetch (Maybe MakefileTemplateName)
  | Vault FilePath
  | Deactivate
  | Hook Shell
  | Export Shell

readEnvironmentType :: String -> Maybe EnvironmentType
readEnvironmentType "prod" = Just Prod
readEnvironmentType "staging" = Just Staging
readEnvironmentType s = Just $ Other s

versionOpt =
  infoOption
    (showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

passPathOpt =
  optional $
  strOption
    (long "password-store-path" <> short 'p' <> metavar "PATH" <>
     help "Full path to password store directory.")

makeOpt =
  optional $
  strOption
    (long "makefile" <> short 'm' <> metavar "NAME" <>
     help "Makefile template to fetch from github repo.")

kubeNamespaceOpt =
  optional $
  strOption
    (long "kube-namespace" <> short 'n' <> metavar "NAMESPACE" <>
     help "Kube Namespace. Example: kube-system or default.")

kubeProjectOpt =
  strOption
    (long "kube-project" <> short 'p' <> metavar "YAMLPATH" <>
     help "Full path to kube config yaml file.")

vaultProjectOpt =
  strOption
    (long "vault-project" <> short 'p' <> metavar "PATH" <>
     help "Vault env file path")

cmdFetch = command "fetch" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Fetches various templates."
    options = Fetch <$> makeOpt

cmdVault = command "vault" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set vault environment."
    options = Vault <$> vaultProjectOpt

cmdTerraform = command "tf" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set terraform environment."
    options =
      Terraform <$> argument (maybeReader readEnvironmentType) (metavar "ENV")

cmdKube = command "kube" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set kube environment."
    options = Kube <$> kubeProjectOpt <*> kubeNamespaceOpt

cmdPass = command "pass" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set pass environment."
    options = Pass <$> passPathOpt

cmdDeactivate = command "deactivate" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Deactivate environment."
    options = pure Deactivate

cmdHook = command "hook" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Used to setupt the shell hook."
    options = Hook <$> argument auto (metavar "SHELL")

cmdExport = command "export" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Exports the needed environment variables. Used internally."
    options = Export <$> argument auto (metavar "SHELL")

argCmds =
  subparser
    (cmdKube <> cmdPass <> cmdTerraform <> cmdFetch <> cmdVault <> cmdDeactivate <>
     cmdHook <>
     cmdExport)

denvArgs :: Parser DenvArgs
denvArgs = DenvArgs <$> argCmds
