{-# LANGUAGE OverloadedStrings #-}

module Denv.Options where

import RIO

import qualified RIO.Text as T
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Denv.Types
import Denv.Aws.Types
import Options.Applicative
import Paths_denv (version)

data DenvArgs = DenvArgs
  { denvCommand :: Command
  , debug :: Bool
  }

data Command
  = Kube KubeProjectName
         (Maybe KubeNamespace)
  | Pass (Maybe PasswordStorePath)
  | Terraform EnvironmentType
  | Source FilePath
  | Aws AwsProfile [String]
  | Vault FilePath
  | Deactivate
  | Hook Shell
  | Export Shell

type Env = [(String, String)]

environ :: (HasValue f) => (String -> Maybe a) -> String -> Env -> Mod f a
environ r k env = maybe idm value $ r =<< lookup k env

readAwsProfile :: String -> Maybe AwsProfile
readAwsProfile s = Just $ AwsProfile $ T.pack s

readEnvironmentType :: String -> Maybe EnvironmentType
readEnvironmentType "prod" = Just Prod
readEnvironmentType "staging" = Just Staging
readEnvironmentType s = Just $ Other s

debugSwitch :: Parser Bool
debugSwitch = switch (long "debug" <> help "Debug mode. Verbose output.")

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption
    (showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

passPathOpt :: Parser (Maybe String)
passPathOpt =
  optional $
  strOption
    (long "password-store-path" <> short 'p' <> metavar "PATH" <>
     help "Full path to password store directory.")

kubeNamespaceOpt :: Parser (Maybe String)
kubeNamespaceOpt =
  optional $
  strOption
    (long "kube-namespace" <> short 'n' <> metavar "NAMESPACE" <>
     help "Kube Namespace. Example: kube-system or default.")

kubeProjectOpt :: Parser String
kubeProjectOpt =
  strOption
    (long "kube-project" <> short 'p' <> metavar "YAMLPATH" <>
     help "Full path to kube config yaml file.")

envFilePathOpt :: Parser String
envFilePathOpt =
  argument str
    (metavar "PATH" <>
     help "Raw env file path")

execCommandArg :: Parser [String]
execCommandArg = many $ strArgument (metavar "CMD [ARGS]" <> help "Command and arguments to run.")

awsProfileOpt :: Env -> Parser AwsProfile
awsProfileOpt env =
  option (maybeReader readAwsProfile)
    (long "profile" <> short 'p' <> metavar "PROFILE" <> environ readAwsProfile "AWS_DEFAULT_PROFILE" env <>
     help "Aws profile to use. Must be defined in ~/.aws/config.")

vaultProjectOpt :: Parser String
vaultProjectOpt =
  strOption
    (long "vault-project" <> short 'p' <> metavar "PATH" <>
     help "Vault env file path")

cmdSource :: Mod CommandFields Command
cmdSource = command "source" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Source env file."
    options = Source <$> envFilePathOpt

cmdVault :: Mod CommandFields Command
cmdVault = command "vault" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set vault environment."
    options = Vault <$> vaultProjectOpt

cmdTerraform :: Mod CommandFields Command
cmdTerraform = command "tf" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set terraform environment."
    options =
      Terraform <$> argument (maybeReader readEnvironmentType) (metavar "ENV")

cmdAws :: Env -> Mod CommandFields Command
cmdAws env = command "aws" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set AWS environment. This feature is in BETA."
    options =
      Aws <$> awsProfileOpt env <*> execCommandArg

cmdKube :: Mod CommandFields Command
cmdKube = command "kube" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set kube environment."
    options = Kube <$> kubeProjectOpt <*> kubeNamespaceOpt

cmdPass :: Mod CommandFields Command
cmdPass = command "pass" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set pass environment."
    options = Pass <$> passPathOpt

cmdDeactivate :: Mod CommandFields Command
cmdDeactivate = command "deactivate" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Deactivate environment."
    options = pure Deactivate

cmdHook :: Mod CommandFields Command
cmdHook = command "hook" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Used to setupt the shell hook."
    options = Hook <$> argument auto (metavar "SHELL")

cmdExport :: Mod CommandFields Command
cmdExport = command "export" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Exports the needed environment variables. Used internally."
    options = Export <$> argument auto (metavar "SHELL")

argCmds :: Env -> Parser Command
argCmds env =
  subparser
    (cmdKube <> cmdPass <> cmdTerraform <> cmdVault <> cmdDeactivate <>
     cmdSource <>
     cmdHook <>
     cmdAws env <>
     cmdExport)

denvArgs :: Env -> Parser DenvArgs
denvArgs env = DenvArgs <$> argCmds env <*> debugSwitch
