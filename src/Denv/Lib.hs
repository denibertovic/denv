{-# LANGUAGE OverloadedStrings #-}

module Denv.Lib where

import Control.Monad (mapM_, unless, when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client.Conduit.Download (download)
import System.Directory
       (doesDirectoryExist, doesFileExist, getHomeDirectory, removeFile,
        renameFile)
import System.Directory (getCurrentDirectory)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnv, lookupEnv, unsetEnv)
import System.Exit (die, exitSuccess)
import System.FilePath ((</>))
import System.FilePath.Posix
       (splitPath, takeBaseName, takeFileName)

import Denv.Options
import Denv.Types

entrypoint :: DenvArgs -> IO ()
entrypoint (DenvArgs (Kube p n)) = mkKubeEnv p n
entrypoint (DenvArgs (Pass p)) = mkPassEnv p
entrypoint (DenvArgs (Fetch makefile)) = fetchTemplate makefile
entrypoint (DenvArgs (Vault p)) = mkVaultEnv p
entrypoint (DenvArgs (Terraform e)) = mkTerraformEnv e
entrypoint (DenvArgs Deactivate) = deactivateEnv
entrypoint (DenvArgs (Hook s)) = execHook s
entrypoint (DenvArgs (Export s)) = execExport s

ps1 :: String
ps1 = "\"$PS1\""

mkVaultEnv :: FilePath -> IO ()
mkVaultEnv p = do
  exists <- doesFileExist p
  unless exists (die $ "ERROR: VaultConfig file does not exist: " ++ p)
  let p' = takeFileName p
  c <- TIO.readFile p
  h <- getHomeDirectory
  let rc = h </> ".denv"
  TIO.writeFile rc c
  let env =
        [ EnvVar OldPrompt ps1
        , EnvVar VaultConfig p'
        , EnvVar Prompt "\"vault|$VAULT_CONFIG $PS1\""
        ]
  TIO.appendFile rc (toEnv env)

mkTerraformEnv :: EnvironmentType -> IO ()
mkTerraformEnv e = do
  curDirPath <- getCurrentDirectory
  let path = curDirPath </> show e
  exists <- doesDirectoryExist path
  unless exists (die $ "ERROR: Directory does not exist " ++ path)
  c <- TIO.readFile (path </> "env")
  h <- getHomeDirectory
  let rc = h </> ".denv"
  TIO.writeFile rc c
  let env =
        [EnvVar OldPrompt ps1, EnvVar Prompt "\"terraform|$ENVIRONMENT $PS1\""]
  TIO.appendFile rc (toEnv env)

mkPassEnv :: Maybe PasswordStorePath -> IO ()
mkPassEnv p = do
  curDirPath <- getCurrentDirectory
  let p' = fromMaybe curDirPath p
  h <- getHomeDirectory
  let rc = h </> ".denv"
  exists <- doesDirectoryExist p'
  unless exists (die $ "ERROR: Password store does not exist: " ++ p')
  let xs = splitPath p'
  let p'' = intercalate "" $ drop (length xs - 2) xs
  let env =
        [ EnvVar PasswordStoreDir p'
        , EnvVar PasswordStoreDirShort p''
        , EnvVar OldPrompt ps1
        , EnvVar Prompt "\"pass|$PASSWORD_STORE_DIR_SHORT $PS1\""
        ]
  TIO.writeFile rc (toEnv env)

mkKubeEnv :: KubeProjectName -> Maybe KubeNamespace -> IO ()
mkKubeEnv p n = do
  exists <- doesFileExist p
  unless exists (die $ "ERROR: Kubeconfig does not exist: " ++ p)
  h <- getHomeDirectory
  let rc = h </> ".denv"
  let p' = takeFileName p
  let n' = fromMaybe "default" n
  let env =
        [ EnvVar KubeConfig p
        , EnvVar KubeConfigShort p'
        , EnvVar KubectlNamespace n'
        , EnvVar OldPrompt ps1
        , EnvVar Prompt "\"k8s|$KUBECTL_NAMESPACE|$KUBECONFIG_SHORT $PS1\""
        ]
  TIO.writeFile rc (toEnv env)

fetchTemplate :: Maybe MakefileTemplateName -> IO ()
fetchTemplate m = do
  let baseUrl =
        "https://raw.githubusercontent.com/denibertovic/makefiles/master/"
  case m of
    Nothing ->
      die
        "Please specify a template name. See here for a list of templates: https://github.com/denibertovic/makefiles"
    Just name -> do
      exists <- doesFileExist "Makefile"
      when exists (renameFile "Makefile" "Makefile.old")
      download (baseUrl <> name <> ".makefile") "Makefile"

deactivateEnv :: IO ()
deactivateEnv = do
  h <- getHomeDirectory
  let rc = h </> ".denv"
  oldPrompt <- lookupEnv "_OLD_DENV_PS1"
  let restorePrompt = [EnvVar Prompt "\"$_OLD_DENV_PS1\""]
  let env =
        [ Unset OldPrompt
        , Unset KubeConfig
        , Unset KubeConfigShort
        , Unset KubectlNamespace
        , Unset PasswordStoreDir
        , Unset PasswordStoreDirShort
        , Unset VaultConfig
        , Unset VaultAddr
        , Unset VaultToken
        , Unset VaultSkipVerify
        ]
    -- We only restore the prompt if oldPrompt is present otherwise
    -- we would set the prompt to nothing (empty string). This makes
    -- the deactivate command idempotent.
  case oldPrompt of
    Nothing -> do
      TIO.writeFile rc (toEnv env)
      TIO.appendFile rc (unset "ENVIRONMENT")
    Just _ -> do
      TIO.writeFile rc (toEnv $ restorePrompt ++ env)
      TIO.appendFile rc (unset "ENVIRONMENT")

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
