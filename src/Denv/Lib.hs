module Denv.Lib where

import           Control.Monad         (mapM_, unless, when)
import           Data.Text             as T
import           System.Directory      (doesFileExist)
import           System.Directory      (getCurrentDirectory)
import           System.Environment    (getEnv, unsetEnv)
import           System.Exit           (die)
import           System.FilePath       ((</>))
import           System.FilePath.Posix (takeBaseName, takeFileName)
import           Text.Printf           (printf)

import           Denv.Options
import           Denv.Types

entrypoint :: DenvArgs -> IO ()
entrypoint (DenvArgs (Kube p n)) = mkKubeEnv p n
entrypoint (DenvArgs Deactivate) = deactivateEnv


mkKubeEnv :: KubeProjectName -> KubeNamespace -> IO ()
mkKubeEnv p n = do
    exists <- doesFileExist p
    unless exists (die $ "ERROR: Kubeconfig does not exits: " ++ p)
    let p' = takeFileName p
    putStrLn $ set $ EnvVar "KUBECONFIG" p
    putStrLn $ set $ EnvVar "KUBECONFIG_SHORT" p'
    putStrLn $ set $ EnvVar "KUBECTL_NAMESPACE" n
    putStrLn $ set $ EnvVar "_OLD_DENV_PS1" "\"$PS1\""
    putStrLn $ set $ EnvVar "PS1" "\"%F{blue}k8s%f%F{blue}%f|%F{red}$KUBECTL_NAMESPACE%f|%F{red}$KUBECONFIG_SHORT%f $PS1\""


deactivateEnv :: IO ()
deactivateEnv = do
    putStrLn $ set $ EnvVar "PS1" "\"$_OLD_DENV_PS1\""
    putStrLn $ unset $ EnvVar "_OLD_DENV_PS1" ""
    putStrLn $ unset $ EnvVar "KUBECONFIG" ""
    putStrLn $ unset $ EnvVar "KUBECONFIG_SHORT" ""
    putStrLn $ unset $ EnvVar "KUBECTL_NAMESPACE" ""

data EnvVar = EnvVar String String

class Envify a where
    set :: a -> String
    unset :: a -> String

instance Envify EnvVar where
    set (EnvVar k v) = "export " ++ k ++ "=" ++ v ++ ";"
    unset (EnvVar k _) = "unset " ++ k ++ ";"

