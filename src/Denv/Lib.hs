{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Denv.Lib where

import RIO

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude (putStrLn)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getHomeDirectory
  , removeFile
  )
import System.Environment (lookupEnv)
import System.Exit (die, exitSuccess)
import System.FilePath ((</>))
import System.FilePath.Posix (takeFileName)

import Denv.Options
import Denv.Types
import Denv.Utils
import qualified Denv.Aws as AWS

entrypoint :: DenvArgs -> IO ()
entrypoint (DenvArgs (Kube p n t) debug) = mkKubeEnv p n t
entrypoint (DenvArgs (Gcp p) debug) = mkGcpEnv p
entrypoint (DenvArgs (Pass p) debug) = mkPassEnv p
entrypoint (DenvArgs (Source l p) debug) = mkRawEnv l p
entrypoint (DenvArgs (Aws p cmd) debug) = AWS.mkAwsEnv p cmd debug
entrypoint (DenvArgs Deactivate debug) = deactivateEnv
entrypoint (DenvArgs (Hook s) debug) = execHook s
entrypoint (DenvArgs (Export s) debug) = execExport s

mkRawEnv :: Maybe String -> FilePath -> IO ()
mkRawEnv mp p = do
  checkEnv
  exists <- doesFileExist p
  unless exists (die $ "ERROR: File does not exist: " ++ p)
  let p' = mkNameShort p
  c <- TIO.readFile p
  deac <- parseEnvFileOrDie p' c
  let prp = T.pack $ maybe "raw|" (\x -> x <> "|") mp
  let env =
        withVarTracking
          (Just deac)
          [ Set OldPrompt ps1
          , Set RawEnvFile $ T.pack p'
          , Set Prompt $ mkEscapedText $ prp <> "$RAW_ENV_FILE $PS1"
          ]
  writeRcWithPredefined c env

mkPassEnv :: Maybe PasswordStorePath -> IO ()
mkPassEnv p = do
  checkEnv
  curDirPath <- getCurrentDirectory
  let p' = fromMaybe curDirPath p
  exists <- doesDirectoryExist p'
  unless exists (die $ "ERROR: Password store does not exist: " ++ p')
  let p'' = mkNameShort p'
  let env =
        withVarTracking
          Nothing
          [ Set PasswordStoreDir $ T.pack p'
          , Set PasswordStoreDirShort $ T.pack p''
          , Set OldPrompt ps1
          , Set Prompt $ mkEscapedText "pass|$PASSWORD_STORE_DIR_SHORT $PS1"
          ]
  writeRc env

mkGcpEnv :: GoogleCredentialsPath  -> IO ()
mkGcpEnv p = do
  checkEnv
  exists <- doesFileExist p
  unless exists (die $ "ERROR: Google credentials file does not exist: " ++ p)
  let p' = mkNameShort p
  let env =
        withVarTracking
          Nothing
          [ Set GoogleCredentials $ T.pack p
          , Set GoogleCredentialsShort $ T.pack p'
          , Set OldPrompt ps1
          , Set Prompt $ mkEscapedText "gcp|$GOOGLE_CREDENTIALS_SHORT $PS1"
          ]
  writeRc env

mkKubeEnv :: KubeProjectName -> Maybe KubeNamespace -> Maybe TillerNamespace -> IO ()
mkKubeEnv p n t = do
  checkEnv
  exists <- doesFileExist p
  unless exists (die $ "ERROR: Kubeconfig does not exist: " ++ p)
  let p' = takeFileName p
  let n' = fromMaybe "default" n
  let t' = fromMaybe "kube-system" t
  let env =
        withVarTracking
          Nothing
          [ Set KubeConfig $ T.pack p
          , Set KubeConfigShort $ T.pack p'
          , Set KubectlNamespace $ T.pack n'
          , Set TillerNamespace $ T.pack t'
          , Set OldPrompt ps1
          , Set Prompt $
            mkEscapedText $ "k8s|n:$KUBECTL_NAMESPACE|t:$TILLER_NAMESPACE|$KUBECONFIG_SHORT $PS1"
          ]
  writeRc env

deactivateEnv :: IO ()
deactivateEnv = do
  toDeactivate <- lookupEnv "_DENV_SET_VARS"
  let restorePrompt = [Set Prompt "\"$_OLD_DENV_PS1\""]
  -- We only restore the prompt if oldPrompt is present otherwise
  -- we would set the prompt to nothing (empty string). This makes
  -- the deactivate command idempotent.
  case toDeactivate of
    Nothing -> return ()
    Just t -> do
      putStrLn $
        T.unpack $ "denv unsetting: " <> (T.replace "," " / " (T.pack t))
      let env :: [DenvVariable]
          env = map Unset $ T.splitOn "," (T.pack t)
      writeRc (restorePrompt <> env)

execHook :: Shell -> IO ()
execHook BASH = putStrLn bashHook
execHook ZSH = putStrLn zshHook

execExport :: Shell -> IO ()
execExport _ = do
  h <- getHomeDirectory
  let rc = h </> ".denv"
  exists <- doesFileExist rc
  unless exists exitSuccess
  exports <- TIO.readFile rc
  putStrLn (T.unpack exports)
  removeFile rc

-- This is inspired by [direnv](https://github.com/direnv/direnv) (all credits to the authors).
-- LICENCE: https://github.com/direnv/direnv/blob/80adc42d27ce1048eb7829e1150691c967b3d60e/LICENSE.md
zshHook :: String
zshHook =
  unlines
    [ "_denv_hook() {"
    , "  eval \"$(denv export ZSH)\";"
    , "}"
    , "typeset -ag precmd_functions;"
    , "if [[ -z ${precmd_functions[(r)_denv_hook]} ]]; then"
    , "  precmd_functions+=_denv_hook;"
    , "fi"
    ]

-- This is inspired by [direnv](https://github.com/direnv/direnv) (all credits to the authors).
-- LICENCE: https://github.com/direnv/direnv/blob/80adc42d27ce1048eb7829e1150691c967b3d60e/LICENSE.md
bashHook :: String
bashHook =
  unlines
    [ "_denv_hook() {"
    , "  local previous_exit_status=$?;"
    , "  eval \"$(denv export BASH)\";"
    , "  return $previous_exit_status;"
    , "};"
    , "if ! [[ \"$PROMPT_COMMAND\" =~ _denv_hook ]]; then"
    , "  PROMPT_COMMAND=\"_denv_hook;$PROMPT_COMMAND\";"
    , "fi"
    ]
