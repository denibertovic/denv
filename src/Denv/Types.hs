{-# LANGUAGE OverloadedStrings #-}

module Denv.Types where

type KubeProjectName = String
type KubeNamespace = String
type PasswordStorePath = String

data Shell = BASH | ZSH deriving (Show, Eq, Read)

