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
import           System.Environment    (getEnv, unsetEnv)
import           System.Exit           (die, exitSuccess)
import           System.FilePath       ((</>))
import           System.FilePath.Posix (splitPath, takeBaseName, takeFileName)
import           Text.Printf           (printf)

import           Denv.Options
import           Denv.Types

entrypoint :: DenvArgs -> IO ()
entrypoint (DenvArgs (Kube p n)) = mkKubeEnv p n
entrypoint (DenvArgs (Pass p))   = mkPassEnv p
entrypoint (DenvArgs Deactivate) = deactivateEnv
entrypoint (DenvArgs (Hook s))   = execHook s
entrypoint (DenvArgs (Export s)) = execExport s

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
    TIO.writeFile rc (set $ EnvVar "PASSWORD_STORE_DIR" p')
    TIO.appendFile rc (set $ EnvVar "PASSWORD_STORE_DIR_SHORT" p'')
    TIO.appendFile rc (set $ EnvVar "_OLD_DENV_PS1" "\"$PS1\"")
    TIO.appendFile rc (set $ EnvVar "PS1" "\"pass|$PASSWORD_STORE_DIR_SHORT $PS1\"")

mkKubeEnv :: KubeProjectName -> Maybe KubeNamespace -> IO ()
mkKubeEnv p n = do
    exists <- doesFileExist p
    unless exists (die $ "ERROR: Kubeconfig does not exist: " ++ p)
    h <- getHomeDirectory
    let rc = h </> ".denv"
    let p' = takeFileName p
    let n' = fromMaybe "default" n
    TIO.writeFile rc (set $ EnvVar "KUBECONFIG" p)
    TIO.appendFile rc (set $ EnvVar "KUBECONFIG_SHORT" p')
    TIO.appendFile rc (set $ EnvVar "KUBECTL_NAMESPACE" n')
    TIO.appendFile rc (set $ EnvVar "_OLD_DENV_PS1" "\"$PS1\"")
    TIO.appendFile rc (set $ EnvVar "PS1" "\"k8s|$KUBECTL_NAMESPACE|$KUBECONFIG_SHORT $PS1\"")


deactivateEnv :: IO ()
deactivateEnv = do
    h <- getHomeDirectory
    let rc = h </> ".denv"
    TIO.writeFile rc (set $ EnvVar "PS1" "\"$_OLD_DENV_PS1\"")
    TIO.appendFile rc (unset $ EnvVar "_OLD_DENV_PS1" "")
    TIO.appendFile rc (unset $ EnvVar "KUBECONFIG" "")
    TIO.appendFile rc (unset $ EnvVar "KUBECONFIG_SHORT" "")
    TIO.appendFile rc (unset $ EnvVar "KUBECTL_NAMESPACE" "")
    TIO.appendFile rc (unset $ EnvVar "PASSWORD_STORE_DIR" "")
    TIO.appendFile rc (unset $ EnvVar "PASSWORD_STORE_DIR_SHORT" "")

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

data EnvVar = EnvVar String String

class Envify a where
    set :: a -> T.Text
    unset :: a -> T.Text

instance Envify EnvVar where
    set (EnvVar k v) = T.pack $ "export " ++ k ++ "=" ++ v ++ ";" ++ "\n"
    unset (EnvVar k _) = T.pack $ "unset " ++ k ++ ";" ++ "\n"

