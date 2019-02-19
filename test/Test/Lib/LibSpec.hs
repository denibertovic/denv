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
        _ <- mkKubeEnv f Nothing Nothing
        cont <- TIO.readFile (d </> ".denv")
        let config = "export KUBECONFIG=" <> T.pack f <> ";"
        let short = "export KUBECONFIG_SHORT=foobar;"
        let namespace = "export KUBECTL_NAMESPACE=default;"
        let tillerNamespace = "export TILLER_NAMESPACE=kube-system;"
        let ls = T.lines cont
        (config `elem` ls) `shouldBe` True
        (namespace `elem` ls) `shouldBe` True
        (tillerNamespace `elem` ls) `shouldBe` True
        (short `elem` ls) `shouldBe` True
    it "Tests kube env with namespace" $ do
      withRandomTempFile $ \d f -> do
        setEnv "HOME" d
        _ <- mkKubeEnv f (Just "kube-system") (Just "testing")
        cont <- TIO.readFile (d </> ".denv")
        let config = "export KUBECONFIG=" <> T.pack f <> ";"
        let short = "export KUBECONFIG_SHORT=foobar;"
        let namespace = "export KUBECTL_NAMESPACE=kube-system;"
        let tillerNamespace = "export TILLER_NAMESPACE=testing;"
        let ls = T.lines cont
        (config `elem` ls) `shouldBe` True
        (namespace `elem` ls) `shouldBe` True
        (tillerNamespace `elem` ls) `shouldBe` True
        (short `elem` ls) `shouldBe` True
    it "Tests vault env" $ do
      withTempVaultConfig $ \d f -> do
        setEnv "HOME" d
        _ <- mkVaultEnv f
        cont <- TIO.readFile (d </> ".denv")
        (testVaultConfig `isPrefixOf` T.lines cont) `shouldBe` True
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
              , "export PASSWORD_STORE_DIR_SHORT=" <> (T.pack $ mkPassDirShort d) <> ";"
              ]
        (ret `isPrefixOf` T.lines cont) `shouldBe` True
    it "Tests tf env" $ do
      withTempTerraformConfig $ \d f -> do
        setEnv "HOME" d
        setCurrentDirectory d
        _ <- mkTerraformEnv f
        cont <- TIO.readFile (d </> ".denv")
        (testTerraformConfig `isPrefixOf` T.lines cont) `shouldBe` True
    it "Tests deactivate env" $ do
      withTempTerraformConfig $ \d _ -> do
        setEnv "HOME" d
        setEnv "_DENV_SET_VARS" "FOO,BAR,_DENV_SET_VARS"
        _ <- deactivateEnv
        cont <- TIO.readFile (d </> ".denv")
        let expected = "export PS1=\"$_OLD_DENV_PS1\";\nunset \"FOO\";\nunset \"BAR\";\nunset \"_DENV_SET_VARS\";\n"
        (expected == cont) `shouldBe` True
    it "Tests tracking vars works" $ do
      let env = [ Set OldPrompt ps1
                , Set VaultConfig "foobar"
                , Set Prompt $ mkEscapedText "vault|$VAULT_CONFIG $PS1"
                ]
      let expected = Set DenvSetVars "_OLD_DENV_PS1,VAULT_CONFIG,_DENV_SET_VARS"
      let ret = withVarTracking Nothing env
      (expected `elem` ret) `shouldBe` True
      let predefined = "FOO,"
      let ret2 = withVarTracking (Just predefined) env
      let expected2 = Set DenvSetVars "_OLD_DENV_PS1,VAULT_CONFIG,FOO,_DENV_SET_VARS"
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
  , "export CLUSTER_NAME=foor"
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
