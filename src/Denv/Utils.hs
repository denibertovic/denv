{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Denv.Utils where

import RIO

import System.Environment (lookupEnv)
import System.Exit (die)
import Data.List (foldl, intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified LoadEnv.Parse as LE
import System.FilePath ((</>))
import Text.Parsec (parse)
import System.FilePath.Posix (splitPath)
import System.Directory ( getHomeDirectory )

import Denv.Types

mkEscapedText :: T.Text -> T.Text
mkEscapedText x = "\"" <> x <> "\""

withVarTracking :: Maybe T.Text -> [DenvVariable] -> [DenvVariable]
withVarTracking x xs =
  case x of
    Nothing ->
      xs <>
      [ Set DenvSetVars $ (texify xs) <> "," <> appendTracking
      ]
    Just x' ->
      xs <>
      [ Set DenvSetVars $ (texify xs) <> "," <> x' <> appendTracking
      ]
  where
    getText :: DenvVariable -> T.Text
    getText x' = case x' of
      (Set v _) ->T.pack $ show v
      (Unset _) -> ""
    nonEmpty e = e /= ""
    -- We don't want to unset PS1 as that would leave us with an empty prompt.
    -- Hence we filter it out of the vars we're tracking for deactivation
    isps1 e = e /= "PS1"
    appendTracking = (T.pack $ show DenvSetVars)
    texify :: [DenvVariable] -> T.Text
    texify xs' = T.intercalate "," $ filter isps1 $ filter nonEmpty $ map getText xs'

checkEnv :: IO ()
checkEnv = do
  isActiveEnv <- lookupEnv "_DENV_SET_VARS"
  case isActiveEnv of
    Nothing -> return ()
    Just _ -> die "Env already active. Please run `deactivate` first."

envify :: DenvVariable -> T.Text
envify (Set k v) = "export " <> (T.pack $ show k) <> "=" <> v <> ";" <> "\n"
envify (Unset k) = "unset " <> (T.pack $ show k) <> ";" <> "\n"

toEnv :: [DenvVariable] -> T.Text
toEnv xs = T.intercalate "" (map envify xs)

writeRc :: [DenvVariable] -> IO ()
writeRc env = do
  h <- getHomeDirectory
  let rc = h </> ".denv"
  TIO.writeFile rc (toEnv env)

writeRcWithPredefined :: T.Text -> [DenvVariable] -> IO ()
writeRcWithPredefined c env = do
  h <- getHomeDirectory
  let rc = h </> ".denv"
  TIO.writeFile rc c
  TIO.appendFile rc (toEnv env)

parseEnvFileOrDie :: FilePath -> T.Text -> IO T.Text
parseEnvFileOrDie p c = do
  let deac = parse LE.parseEnvironment p (T.unpack c)
  case deac of
    Left err -> die $ show err
    Right deac' -> do
      let deac'' =
            foldl
              (\acc (k :: String, _ :: String) -> (T.pack k) <> "," <> acc)
              ""
              deac'
      return deac''

mkPassDirShort :: FilePath -> String
mkPassDirShort xs = intercalate "" $ drop (length xs - 2) $ splitPath xs
