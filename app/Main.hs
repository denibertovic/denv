{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO

import Data.Semigroup ((<>))
import Denv.Lib
import Denv.Options
import Options.Applicative
import System.Environment (getEnvironment)

main :: IO ()
main = do
  env <- getEnvironment
  execParser (opts env) >>= entrypoint
  where
    opts env =
      info
        (helper <*> versionOpt <*> denvArgs env)
        (fullDesc <> progDesc "denv - A tool to help manage environments." <>
         header "denv - A tool to help manage environments.")
