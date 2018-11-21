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

entrypoint :: DenvArgs -> IO ()
entrypoint (DenvArgs (Kube p n)) = mkKubeEnv p n
entrypoint (DenvArgs (Pass p)) = mkPassEnv p
entrypoint (DenvArgs (Source p)) = mkRawEnv p
entrypoint (DenvArgs (Vault p)) = mkVaultEnv p
entrypoint (DenvArgs (Terraform e)) = mkTerraformEnv e
entrypoint (DenvArgs Deactivate) = deactivateEnv
entrypoint (DenvArgs (Hook s)) = execHook s
entrypoint (DenvArgs (Export s)) = execExport s

ps1 :: T.Text
ps1 = mkEscapedText "$PS1"

mkRawEnv :: FilePath -> IO ()
mkRawEnv p = do
  checkEnv
  exists <- doesFileExist p
  unless exists (die $ "ERROR: File does not exist: " ++ p)
  let p' = mkRawEnvShort p
  c <- TIO.readFile p
  deac <- parseEnvFileOrDie p' c
  let env =
        withVarTracking
          (Just deac)
          [ Set OldPrompt ps1
          , Set RawEnvFile $ T.pack p'
          , Set Prompt $ mkEscapedText "raw|$RAW_ENV_FILE $PS1"
          ]
  writeRcWithPredefined c env


mkVaultEnv :: FilePath -> IO ()
mkVaultEnv p = do
  checkEnv
  exists <- doesFileExist p
  unless exists (die $ "ERROR: VaultConfig file does not exist: " ++ p)
  let p' = takeFileName p
  c <- TIO.readFile p
  deac <- parseEnvFileOrDie p' c
  let env =
        withVarTracking
          (Just deac)
          [ Set OldPrompt ps1
          , Set VaultConfig $ T.pack p'
          , Set Prompt $ mkEscapedText "vault|$VAULT_CONFIG $PS1"
          ]
  writeRcWithPredefined c env

mkTerraformEnv :: EnvironmentType -> IO ()
mkTerraformEnv e = do
  checkEnv
  curDirPath <- getCurrentDirectory
  let path = curDirPath </> show e
  exists <- doesDirectoryExist path
  unless exists (die $ "ERROR: Directory does not exist " ++ path)
  c <- TIO.readFile (path </> "env")
  deac <- parseEnvFileOrDie path c
  let env =
        withVarTracking
          (Just deac)
          [ Set OldPrompt ps1
          , Set Prompt $ mkEscapedText "terraform|$ENVIRONMENT $PS1"
          ]
  writeRcWithPredefined c env

mkPassEnv :: Maybe PasswordStorePath -> IO ()
mkPassEnv p = do
  checkEnv
  curDirPath <- getCurrentDirectory
  let p' = fromMaybe curDirPath p
  exists <- doesDirectoryExist p'
  unless exists (die $ "ERROR: Password store does not exist: " ++ p')
  let p'' = mkPassDirShort p'
  let env =
        withVarTracking
          Nothing
          [ Set PasswordStoreDir $ T.pack p'
          , Set PasswordStoreDirShort $ T.pack p''
          , Set OldPrompt ps1
          , Set Prompt $ mkEscapedText "pass|$PASSWORD_STORE_DIR_SHORT $PS1"
          ]
  writeRc env

mkKubeEnv :: KubeProjectName -> Maybe KubeNamespace -> IO ()
mkKubeEnv p n = do
  checkEnv
  exists <- doesFileExist p
  unless exists (die $ "ERROR: Kubeconfig does not exist: " ++ p)
  let p' = takeFileName p
  let n' = fromMaybe "default" n
  let env =
        withVarTracking
          Nothing
          [ Set KubeConfig $ T.pack p
          , Set KubeConfigShort $ T.pack p'
          , Set KubectlNamespace $ T.pack n'
          , Set OldPrompt ps1
          , Set Prompt $
            mkEscapedText "k8s|$KUBECTL_NAMESPACE|$KUBECONFIG_SHORT $PS1"
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
