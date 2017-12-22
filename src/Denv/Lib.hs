{-# LANGUAGE OverloadedStrings #-}

module Denv.Lib where

import           Control.Monad         (mapM_, unless, when)
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe, maybe)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        getHomeDirectory, removeFile)
import           System.Directory      (getCurrentDirectory)
import           System.Directory      (getCurrentDirectory)
import           System.Environment    (getEnv, lookupEnv, unsetEnv)
import           System.Exit           (die, exitSuccess)
import           System.FilePath       ((</>))
import           System.FilePath.Posix (splitPath, takeBaseName, takeFileName)

import           Denv.Options
import           Denv.Types

entrypoint :: DenvArgs -> IO ()
entrypoint (DenvArgs (Kube p n)) = mkKubeEnv p n
entrypoint (DenvArgs (Pass p))   = mkPassEnv p
entrypoint (DenvArgs Deactivate) = deactivateEnv
entrypoint (DenvArgs (Hook s))   = execHook s
entrypoint (DenvArgs (Export s)) = execExport s

ps1 = "\"$PS1\""

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
    let env = [ EnvVar PasswordStoreDir p'
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
    let env =  [ EnvVar KubeConfig p
               , EnvVar KubeConfigShort p'
               , EnvVar KubectlNamespace n'
               , EnvVar OldPrompt ps1
               , EnvVar Prompt "\"k8s|$KUBECTL_NAMESPACE|$KUBECONFIG_SHORT $PS1\""
               ]
    TIO.writeFile rc (toEnv env)

deactivateEnv :: IO ()
deactivateEnv = do
    h <- getHomeDirectory
    let rc = h </> ".denv"
    oldPrompt <- lookupEnv "_OLD_DENV_PS1"
    let restorePrompt = [EnvVar Prompt "\"$_OLD_DENV_PS1\""]
    let env = [ Unset OldPrompt
              , Unset KubeConfig
              , Unset KubeConfigShort
              , Unset KubectlNamespace
              , Unset PasswordStoreDir
              , Unset PasswordStoreDirShort
              ]
    -- We only restore the prompt if oldPrompt is present otherwise
    -- we would set the prompt to nothing (empty string). This makes
    -- the deactivate command idempotent.
    case oldPrompt of
      Nothing -> do
        TIO.writeFile rc (toEnv env)
      Just _  -> do
        TIO.writeFile rc (toEnv $ restorePrompt ++ env)

execHook :: Shell -> IO ()
execHook BASH = putStrLn bashHook
execHook ZSH  = putStrLn zshHook

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
zshHook = unlines [
  "_denv_hook() {"
  , "  eval \"$(denv export ZSH)\";"
  , "}"
  , "typeset -ag precmd_functions;"
  , "if [[ -z ${precmd_functions[(r)_denv_hook]} ]]; then"
  , "  precmd_functions+=_denv_hook;"
  , "fi"
  ]

-- This is inspired by [direnv](https://github.com/direnv/direnv) (all credits to the authors).
bashHook :: String
bashHook = unlines [
  "_denv_hook() {"
  , "  local previous_exit_status=$?;"
  , "  eval \"$(denv export BASH)\";"
  , "  return $previous_exit_status;"
  , "};"
  , "if ! [[ \"$PROMPT_COMMAND\" =~ _denv_hook ]]; then"
  , "  PROMPT_COMMAND=\"_denv_hook;$PROMPT_COMMAND\";"
  , "fi"
  ]

