{-# LANGUAGE OverloadedStrings #-}

module Denv.Options where

import           Data.Semigroup      ((<>))
import           Denv.Types
import           Options.Applicative


data DenvArgs = DenvArgs { denvCommand :: Command }


data Command = Kube KubeProjectName (Maybe KubeNamespace)
             | Deactivate

kubeNamespaceOpt = optional $ strOption (
                     long "kube-namespace"
                     <> short 'n'
                     <> metavar "NAMESPACE"
                     <> help "Kube Namespace. Example: kube-system or default")

kubeProjectOpt = strOption (
                   long "kube-project"
                   <> short 'p'
                   <> metavar "NAMESPACE"
                   <> help "Full path to kube config yaml file")

cmdKube = command "kube" infos
    where infos = info options desc
          desc = progDesc "Set kube environment."
          options = Kube <$> kubeProjectOpt <*> kubeNamespaceOpt

cmdDeactivate = command "deactivate" infos
    where infos = info options desc
          desc = progDesc "Deactivate environment"
          options = pure Deactivate

argCmds = subparser (cmdKube <> cmdDeactivate)

denvArgs :: Parser DenvArgs
denvArgs = DenvArgs <$> argCmds

