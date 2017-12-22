{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup      ((<>))
import           Denv.Lib
import           Denv.Options
import           Options.Applicative


main :: IO ()
main = execParser opts >>= entrypoint
  where
    opts = info (helper <*> versionOpt <*> denvArgs)
      ( fullDesc
     <> progDesc "denv - A tool to help manage environments."
     <> header "denv - A tool to help manage environments." )

