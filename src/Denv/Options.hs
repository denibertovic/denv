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
         (Maybe TillerNamespace)
  | Pass (Maybe PasswordStorePath)
  | Gcp GoogleCredentialsPath
  | Source (Maybe String) FilePath
  | Aws AwsProfile [String]
  | Deactivate
  | Hook Shell
  | Export Shell

type Env = [(String, String)]

environ :: (HasValue f) => (String -> Maybe a) -> String -> Env -> Mod f a
environ r k env = maybe idm value $ r =<< lookup k env

readAwsProfile :: String -> Maybe AwsProfile
readAwsProfile s = Just $ AwsProfile $ T.pack s

debugSwitch :: Parser Bool
debugSwitch = switch (long "debug" <> help "Debug mode. Verbose output.")

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption
    (showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

gcpCredsOpt :: Parser String
gcpCredsOpt =
  strOption
    (long "google-credentials-path" <> short 'p' <> metavar "PATH" <>
     help "Full path to the google credentials JSON file.")

passPathOpt :: Parser (Maybe String)
passPathOpt =
  optional $
  strOption
    (long "password-store-path" <> short 'p' <> metavar "PATH" <>
     help "Full path to password store directory.")

labelOpt :: Parser (Maybe String)
labelOpt =
  optional $
  strOption
    (long "label" <> short 'l' <> metavar "LABEL" <>
     help "The label to be used in the prompt.")


kubeNamespaceOpt :: Parser (Maybe String)
kubeNamespaceOpt =
  optional $
  strOption
    (long "kube-namespace" <> short 'n' <> metavar "NAMESPACE" <>
     help "Kube Namespace. Example: kube-system or default.")

tillerNamespaceOpt :: Parser (Maybe String)
tillerNamespaceOpt =
  optional $
  strOption
    (long "tiller-namespace" <> short 't' <> metavar "NAMESPACE" <>
     help "Tiller Namespace. Example: kube-system or default.")

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

cmdSource :: Mod CommandFields Command
cmdSource = command "source" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Source env file."
    options = Source <$> labelOpt <*> envFilePathOpt

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
    options = Kube <$> kubeProjectOpt <*> kubeNamespaceOpt <*> tillerNamespaceOpt

cmdPass :: Mod CommandFields Command
cmdPass = command "pass" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set pass environment."
    options = Pass <$> passPathOpt

cmdGcp :: Mod CommandFields Command
cmdGcp = command "gcp" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Set GCP credentials."
    options = Gcp <$> gcpCredsOpt

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
    desc = progDesc "Used to setup the shell hook."
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
    (cmdKube <> cmdPass <> cmdDeactivate <>
     cmdSource <>
     cmdHook <>
     cmdAws env <>
     cmdGcp <>
     cmdExport)

denvArgs :: Env -> Parser DenvArgs
denvArgs env = DenvArgs <$> argCmds env <*> debugSwitch
