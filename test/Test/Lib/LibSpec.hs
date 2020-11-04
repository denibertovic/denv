{-# LANGUAGE OverloadedStrings #-}

module Test.Lib.LibSpec where

import RIO

import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.Directory (setCurrentDirectory, createDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

import Denv.Lib
import Denv.Types
import Denv.Utils

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Lib" $ do
    it "Tests kube env" $ do
      withRandomTempFile $ \d f -> do
        setEnv "HOME" d
        _ <- mkKubeEnv f Nothing
        cont <- TIO.readFile (d </> ".denv")
        let config = "export KUBECONFIG=" <> T.pack f <> ";"
        let short = "export KUBECONFIG_SHORT=foobar;"
        let namespace = "export KUBECTL_NAMESPACE=default;"
        let ls = T.lines cont
        (config `elem` ls) `shouldBe` True
        (namespace `elem` ls) `shouldBe` True
        (short `elem` ls) `shouldBe` True
    it "Tests kube env sets denv vars" $ do
      withRandomTempFile $ \d f -> do
        setEnv "HOME" d
        _ <- mkKubeEnv f Nothing
        cont <- TIO.readFile (d </> ".denv")
        let prompt = "export PS1=$_DENV_PROMPT$PS1;"
        let oldPrompt = "export _OLD_DENV_PS1=\"$PS1\";"
        let denvPrompt = "export _DENV_PROMPT=\"k8s|n:$KUBECTL_NAMESPACE|$KUBECONFIG_SHORT \";"
        let denvSetVars = "export _DENV_SET_VARS=KUBECONFIG,KUBECONFIG_SHORT,KUBECTL_NAMESPACE,_OLD_DENV_PS1,_DENV_PROMPT,_DENV_SET_VARS;"
        let ls = T.lines cont
        (prompt `elem` ls) `shouldBe` True
        (oldPrompt `elem` ls) `shouldBe` True
        (denvPrompt `elem` ls) `shouldBe` True
        (denvSetVars `elem` ls) `shouldBe` True
    it "Tests kube env with namespace" $ do
      withRandomTempFile $ \d f -> do
        setEnv "HOME" d
        _ <- mkKubeEnv f (Just "kube-system")
        cont <- TIO.readFile (d </> ".denv")
        let config = "export KUBECONFIG=" <> T.pack f <> ";"
        let short = "export KUBECONFIG_SHORT=foobar;"
        let namespace = "export KUBECTL_NAMESPACE=kube-system;"
        let ls = T.lines cont
        (config `elem` ls) `shouldBe` True
        (namespace `elem` ls) `shouldBe` True
        (short `elem` ls) `shouldBe` True
    it "Tests vault env" $ do
      withTempVaultConfig $ \d f -> do
        setEnv "HOME" d
        _ <- mkRawEnv Nothing f
        cont <- TIO.readFile (d </> ".denv")
        (testVaultConfig `isPrefixOf` T.lines cont) `shouldBe` True
    it "Tests vault env sets denv vars" $ do
      withTempVaultConfig $ \d f -> do
        setEnv "HOME" d
        _ <- mkRawEnv Nothing f
        cont <- TIO.readFile (d </> ".denv")
        let prompt = "export PS1=$_DENV_PROMPT$PS1;"
        let oldPrompt = "export _OLD_DENV_PS1=\"$PS1\";"
        let denvPrompt = "export _DENV_PROMPT=\"raw|$RAW_ENV_FILE \";"
        let denvSetVars = "export _DENV_SET_VARS=_OLD_DENV_PS1,RAW_ENV_FILE,_DENV_PROMPT,VAULT_SKIP_VERIFY,VAULT_TOKEN,VAULT_ADDR,_DENV_SET_VARS;"
        let ls = T.lines cont
        (prompt `elem` ls) `shouldBe` True
        (oldPrompt `elem` ls) `shouldBe` True
        (denvPrompt `elem` ls) `shouldBe` True
        (denvSetVars `elem` ls) `shouldBe` True
    it "Tests sourcing env file" $ do
      withTempVaultConfig $ \d f -> do
        setEnv "HOME" d
        _ <- mkRawEnv Nothing f
        cont <- TIO.readFile (d </> ".denv")
        (testVaultConfig `isPrefixOf` T.lines cont) `shouldBe` True
    it "Tests pass env" $ do
      withRandomTempFile $ \d _ -> do
        setEnv "HOME" d
        _ <- mkPassEnv (Just d)
        cont <- TIO.readFile (d </> ".denv")
        let ret =
              [ "export PASSWORD_STORE_DIR=" <> T.pack d <> ";"
              , "export PASSWORD_STORE_DIR_SHORT=" <> (T.pack $ mkNameShort d) <> ";"
              ]
        (ret `isPrefixOf` T.lines cont) `shouldBe` True
    it "Tests pass env sets denv vars" $ do
      withRandomTempFile $ \d _ -> do
        setEnv "HOME" d
        _ <- mkPassEnv (Just d)
        cont <- TIO.readFile (d </> ".denv")
        let prompt = "export PS1=$_DENV_PROMPT$PS1;"
        let oldPrompt = "export _OLD_DENV_PS1=\"$PS1\";"
        let denvPrompt = "export _DENV_PROMPT=\"pass|$PASSWORD_STORE_DIR_SHORT \";"
        let denvSetVars = "export _DENV_SET_VARS=PASSWORD_STORE_DIR,PASSWORD_STORE_DIR_SHORT,_OLD_DENV_PS1,_DENV_PROMPT,_DENV_SET_VARS;"
        let ls = T.lines cont
        (prompt `elem` ls) `shouldBe` True
        (oldPrompt `elem` ls) `shouldBe` True
        (denvPrompt `elem` ls) `shouldBe` True
        (denvSetVars `elem` ls) `shouldBe` True
    it "Tests tf env" $ do
      withTempTerraformConfig $ \d f -> do
        setEnv "HOME" d
        setCurrentDirectory d
        _ <- mkRawEnv Nothing f
        cont <- TIO.readFile (d </> ".denv")
        (testTerraformConfig `isPrefixOf` T.lines cont) `shouldBe` True
    it "Tests tf env sets denv vars" $ do
      withTempTerraformConfig $ \d f -> do
        setEnv "HOME" d
        setCurrentDirectory d
        _ <- mkRawEnv Nothing f
        cont <- TIO.readFile (d </> ".denv")
        let prompt = "export PS1=$_DENV_PROMPT$PS1;"
        let oldPrompt = "export _OLD_DENV_PS1=\"$PS1\";"
        let denvPrompt = "export _DENV_PROMPT=\"raw|$RAW_ENV_FILE \";"
        let denvSetVars = "export _DENV_SET_VARS=_OLD_DENV_PS1,RAW_ENV_FILE,_DENV_PROMPT,TF_VAR_foo,CLUSTER_NAME,ENVIRONMENT,_DENV_SET_VARS;"
        let ls = T.lines cont
        (prompt `elem` ls) `shouldBe` True
        (oldPrompt `elem` ls) `shouldBe` True
        (denvPrompt `elem` ls) `shouldBe` True
        (denvSetVars `elem` ls) `shouldBe` True
    it "Tests FISH set transformation" $ do
      withTempTerraformConfig $ \d f -> do
        setEnv "HOME" d
        setCurrentDirectory d
        _ <- mkRawEnv Nothing f
        cont <- TIO.readFile (d </> ".denv")
        let ls = T.lines $ fishify cont
        let f1 = "set -x -g CLUSTER_NAME foo"
        let f2 = "set -x -g ENVIRONMENT prod"
        let f3 = "set -x -g TF_VAR_foo bar"
        let prompt = "set -x -g PS1 $_DENV_PROMPT$PS1;"
        let oldPrompt = "set -x -g _OLD_DENV_PS1 \"$PS1\";"
        let denvPrompt = "set -x -g _DENV_PROMPT \"raw|$RAW_ENV_FILE \";"
        let denvSetVars = "set -x -g _DENV_SET_VARS _OLD_DENV_PS1,RAW_ENV_FILE,_DENV_PROMPT,TF_VAR_foo,CLUSTER_NAME,ENVIRONMENT,_DENV_SET_VARS;"
        (f1 `elem` ls) `shouldBe` True
        (f2 `elem` ls) `shouldBe` True
        (f3 `elem` ls) `shouldBe` True
        (prompt `elem` ls) `shouldBe` True
        (oldPrompt `elem` ls) `shouldBe` True
        (denvPrompt `elem` ls) `shouldBe` True
        (denvSetVars `elem` ls) `shouldBe` True
    it "Tests deactivate env" $ do
      withTempTerraformConfig $ \d _ -> do
        setEnv "HOME" d
        setEnv "_DENV_SET_VARS" "FOO,BAR,_DENV_SET_VARS"
        _ <- deactivateEnv
        cont <- TIO.readFile (d </> ".denv")
        let expected = "export PS1=\"$_OLD_DENV_PS1\";\nunset \"FOO\";\nunset \"BAR\";\nunset \"_DENV_SET_VARS\";\n"
        (expected == cont) `shouldBe` True
    it "Tests deactivate env with FISH" $ do
      withTempTerraformConfig $ \d _ -> do
        setEnv "HOME" d
        setEnv "_DENV_SET_VARS" "FOO,BAR,_DENV_SET_VARS"
        _ <- deactivateEnv
        cont <- TIO.readFile (d </> ".denv")
        let expected = "set -x -g PS1 \"$_OLD_DENV_PS1\";\nset -e -g \"FOO\";\nset -e -g \"BAR\";\nset -e -g \"_DENV_SET_VARS\";\n"
        (expected == (fishify cont)) `shouldBe` True
    it "Tests tracking vars works" $ do
      let env = [ Set OldPrompt ps1
                , Set KubeConfig "foobar"
                , Set Prompt $ mkEscapedText "bar | $PS1"
                ]
      let expected = Set DenvSetVars "_OLD_DENV_PS1,KUBECONFIG,_DENV_SET_VARS"
      let ret = withVarTracking Nothing env
      (expected `elem` ret) `shouldBe` True
      let predefined = "FOO,"
      let ret2 = withVarTracking (Just predefined) env
      let expected2 = Set DenvSetVars "_OLD_DENV_PS1,KUBECONFIG,FOO,_DENV_SET_VARS"
      (expected2 `elem` ret2) `shouldBe` True


withRandomTempFile :: (FilePath -> FilePath -> IO a) -> IO a
withRandomTempFile f = do
  withSystemTempDirectory "denv--" $ \d -> do
    let c = d </> "foobar"
    TIO.writeFile c "content doesn't matter"
    f d c

testVaultConfig :: [T.Text]
testVaultConfig =
  [ "export VAULT_ADDR=https://vault.example.com"
  , "export VAULT_TOKEN=secret"
  , "export VAULT_SKIP_VERIFY=true"
  ]

testTerraformConfig :: [T.Text]
testTerraformConfig =
  [ "export ENVIRONMENT=prod"
  , "export CLUSTER_NAME=foo"
  , "export TF_VAR_foo=bar"
  ]

withTempVaultConfig :: (FilePath -> FilePath -> IO a) -> IO a
withTempVaultConfig f = do
  withSystemTempDirectory "denv--" $ \d -> do
    let c = d </> "testvault"
    TIO.writeFile (d </> c) ((T.intercalate "\n" testVaultConfig) <> "\n")
    f d c

withTempTerraformConfig :: (FilePath -> FilePath -> IO a) -> IO a
withTempTerraformConfig f = do
  withSystemTempDirectory "denv--" $ \d -> do
    createDirectory (d </> "prod")
    let c = d </> "prod" </> "env"
    TIO.writeFile (d </> c) ((T.intercalate "\n" testTerraformConfig) <> "\n")
    f d c
