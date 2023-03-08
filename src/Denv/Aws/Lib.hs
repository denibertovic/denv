{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Denv.Aws.Lib where

import Prelude (print)
import RIO
import RIO.Process
import Data.List (intercalate)

import qualified RIO.Map as Map
import System.Process.Typed as TP
import Control.Monad (unless)
import Control.Monad.Trans.Resource
import Data.Ini (Ini(..), lookupValue, readIniFile)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified RIO.ByteString.Lazy as BSL
import Data.Time
  ( defaultTimeLocale
  , formatTime
  , getCurrentTime
  , getCurrentTimeZone
  , utcToLocalTime
  )
import Data.Time.Clock
import Data.Time.Clock.System (systemSeconds, utcToSystemTime)
import qualified Network.AWS as AWS
import Network.AWS.Auth ()
import Network.AWS.Data (fromText, toText)
import qualified Network.AWS.STS.AssumeRole as STS
import qualified Network.AWS.STS.Types as STS
import qualified Network.AWS.STS.GetSessionToken as STS
import System.Directory (doesFileExist, getHomeDirectory)
import System.Exit (die)
import System.Environment (getEnvironment)
import Control.Monad.Trans.Maybe (MaybeT(..))
import System.FilePath ((</>))
import System.Posix.Files (setFileMode, ownerWriteMode, ownerReadMode, unionFileModes)
import Data.Aeson (eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as TLIO

import Denv.Aws.Types
import Denv.Aws.Utils
import Denv.Types
import Denv.Utils

getAwsFileOrDie :: String -> IO (FilePath)
getAwsFileOrDie p = do
  h <- getHomeDirectory
  let f = h </> ".aws" </> p
  exists <- doesFileExist f
  unless exists (die $ "ERROR: File does not exist: " <> f)
  return f

parseConfigOrDie :: IO (Ini)
parseConfigOrDie = do
  configFile <- getAwsFileOrDie "config"
  i <- readIniFile configFile
  either (\_ -> die "ERROR: Failed parsing AWS config file") return i

parseCredentialsOrDie :: IO (Ini)
parseCredentialsOrDie = do
  credsFile <- getAwsFileOrDie "credentials"
  i <- readIniFile credsFile
  either (\_ -> die "ERROR: Failed parsing AWS credentials file") return i

getSourceProfile :: AwsProfile -> Ini -> Either String AwsSourceProfile
getSourceProfile (AwsProfile p) i =
  AwsSourceProfile <$> AwsProfile <$>
  lookupValue ("profile " <> p) "source_profile" i

-- The temporary security credentials created by GetSessionToken can be used to make
-- API calls to any AWS service with the following exceptions:
--   You cannot call any IAM APIs unless MFA authentication information is included in the request.
--   You cannot call any STS API except AssumeRole or GetCallerIdentity .
-- THIS is why we don't allow use of the temporary session tokens unless mfa is present
parseSessionCacheFile :: AwsProfile -> Maybe AwsSourceProfile -> MfaSerial -> IO (Either AwsEnvCacheError AwsEnvSessionCache)
parseSessionCacheFile p sourceProfile (MfaSerial mfa') = do
    fileName <- mkAwsSessionFileName p sourceProfile (MfaSerial mfa')
    exists <- doesFileExist fileName
    if (not exists) then (return $ Left AwsEnvCacheNotFound) else do
      c <- BSL.readFile fileName
      let c' = eitherDecode c :: Either _ AwsEnvSessionCache
      case c' of
        Left _ -> return $ Left AwsEnvCacheParseError
        Right r' -> do
          let exp = fromJust $ sessionCacheExpiration r' -- ^ TODO: fix this
          isExpired <- isSessionCacheExpired exp
          if isExpired then return $ Left AwsEnvCacheExpired
          else return $ Right r'

-- The temporary security credentials created by AssumeRole can be used to make API calls to any
-- AWS service with the following exception: you cannot call the STS service's GetFederationToken or
-- GetSessionToken APIs.
parseRoleCacheFile :: AwsProfile -> RoleArn -> IO (Either AwsEnvCacheError AwsEnvRoleCache)
parseRoleCacheFile p (RoleArn arn') = do
    fileName <- mkAwsRoleFileName p (RoleArn arn')
    exists <- doesFileExist fileName
    if (not exists) then (return $ Left AwsEnvCacheNotFound) else do
      c <- BSL.readFile fileName
      let c' = eitherDecode c :: Either _ AwsEnvRoleCache
      case c' of
        Left _ -> return $ Left AwsEnvCacheParseError
        Right r' -> do
          let exp = fromJust $ roleCacheExpiration r' -- TODO: fix this
          isExpired <- isRoleCacheExpired exp
          if isExpired then return $ Left AwsEnvCacheExpired
          else return $ Right r'

oneHour :: NominalDiffTime
oneHour = fromInteger 3600

-- 36 hours
defaultSessionDurationSeconds :: Integer
defaultSessionDurationSeconds = 129600
defaultSessionRefreshFactor = 65

-- 1 hour
defaultRoleDurationSeconds :: Integer
defaultRoleDurationSeconds = 3600
defaultRoleRefreshFactor = 35

-- Default session_refresh_factor is 65
isSessionCacheExpired exp = do
  now <- getCurrentTime
  let now' = systemSeconds $ utcToSystemTime now
  let exp' = (systemSeconds $ utcToSystemTime exp) - ((fromIntegral defaultSessionDurationSeconds) * defaultSessionRefreshFactor `div` 100)
  if (now' >= exp') then return True else return False

-- Default role_refresh_factor is 35
isRoleCacheExpired exp = do
  now <- getCurrentTime
  let now' = systemSeconds $ utcToSystemTime now
  let exp' = (systemSeconds $ utcToSystemTime exp) - ((fromIntegral defaultRoleDurationSeconds) * defaultRoleRefreshFactor `div` 100)
  if (now' >= exp') then return True else return False

-- Look for key in profile first and then in source profile if it exists
getAwsAccessKey ::
     Maybe AwsSourceProfile -> AwsProfile -> Ini -> Either String AWS.AccessKey
getAwsAccessKey s p i =
  either
    (const $ mkAwsAccessKey <$> lookupValue (fromAwsSourceProfileMaybe s) "aws_access_key_id" i)
    return
    (mkAwsAccessKey <$> lookupValue (fromAwsProfile p) "aws_access_key_id" i)

-- Look for secret in profile first and then in source profile if it exists
getSecretAccessKey ::
     Maybe AwsSourceProfile -> AwsProfile -> Ini -> Either String AWS.SecretKey
getSecretAccessKey s p i =
  either
    (const $ mkAwsSecretKey <$> lookupValue (fromAwsSourceProfileMaybe s) "aws_secret_access_key" i)
    return
    (mkAwsSecretKey <$> lookupValue (fromAwsProfile p) "aws_secret_access_key" i)

-- Look for region in profile and if it's not there use what's in the source profile if it exists
getRegion :: Maybe AwsSourceProfile -> AwsProfile -> Ini -> Either String T.Text
getRegion s p i =
  either
    (const $ lookupValue (mkSPMaybe s) "region" i)
    return
    (lookupValue (mkP p) "region" i)

-- Look for role_arn in profile and if it's not there use what's in the source profile if it exists
getRoleArn ::
     Maybe AwsSourceProfile -> AwsProfile -> Ini -> Either String RoleArn
getRoleArn s p i =
  either
    (const $ RoleArn <$> lookupValue (mkSPMaybe s) "role_arn" i)
    return
    (RoleArn <$> lookupValue (mkP p) "role_arn" i)

-- Look for mfa_serial in profile and if it's not there use what's in the source profile if it exists
getMfaSerial ::
     Maybe AwsSourceProfile -> AwsProfile -> Ini -> Either String MfaSerial
getMfaSerial s p i =
  either
    (const $ MfaSerial <$> lookupValue (mkSPMaybe s) "mfa_serial" i)
    return
    (MfaSerial <$> lookupValue (mkP p) "mfa_serial" i)

getSessionName :: IO (Text)
getSessionName = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  -- $(date +%Y%m%d-%H%M%S)
  return $
    T.pack $
    formatTime defaultTimeLocale "%Y%m%d-%H%M%S" $ utcToLocalTime tz now

getFromRoleCache :: (HasLogFunc env, HasProcessContext env) => AwsProfile -> RoleArn -> RIO env (Maybe AwsEnvAuth)
getFromRoleCache p r = do
  r <- liftIO $ parseRoleCacheFile p r
  case r of
    Left err -> do
      return Nothing
    Right r' -> do
      let exp = fromJust $ roleCacheExpiration r' -- TODO: fix this
      isExpired <- liftIO $ isRoleCacheExpired exp
      if isExpired then return Nothing
      else do
        logDebug "Using role cache..."
        return $ Just $ AwsEnvAuth (roleCacheAccessKeyId r') (roleCacheSecretAccessKey r') (roleCacheSessionToken r')

getFromSessionCache :: (HasLogFunc env, HasProcessContext env) => AwsProfile -> Maybe AwsSourceProfile -> MfaSerial -> RIO env (Maybe AwsEnvAuth)
getFromSessionCache p sp mfa = do
  -- TODO: Add role cache review
  r <- liftIO $ parseSessionCacheFile p sp mfa
  case r of
    Left _ -> return Nothing
    Right r' -> do
      logDebug "Using session cache..."
      return $ Just $ AwsEnvAuth (sessionCacheAccessKeyId r') (sessionCacheSecretAccessKey r') (sessionCacheSessionToken r')

getSTS :: (HasLogFunc env, HasProcessContext env) => AWS.Env -> AWS.Region -> AWS.Logger -> AwsProfile -> Maybe AwsSourceProfile -> MfaSerial -> RIO env (Maybe AwsEnvAuth)
getSTS awsenv region lgr p sourceProfile mfa = do
  logDebug "Getting new STS credentials..."
  sessionName <- liftIO getSessionName
  let gst = STS.getSessionToken
  -- TODO: set duration
  ret <- liftIO $ parseSessionTokenAuthEnvOrDie =<< execStsRequest awsenv lgr region =<< (mkStsSessionTokenRequest mfa gst)
  cacheFileName <- liftIO $ mkAwsSessionFileName p sourceProfile mfa
  liftIO $ writeSessionCache cacheFileName (toSessionCache ret)
  let auth = AwsEnvAuth
              (ret ^. AWS.accessKeyId)
              (ret ^. AWS.secretAccessKey)
              (ret ^. AWS.sessionToken)
  return $ Just auth

-- Use Session Credentials to make an AssumeRole call
-- Only use "Raw" (user) credentials for getting the STS credentials and then use those
-- to assume the role
getSTSWithRole :: (HasLogFunc env, HasProcessContext env) => AWS.Env -> AWS.Region -> AWS.Logger -> AwsProfile -> Maybe AwsSourceProfile -> MfaSerial -> RoleArn -> RIO env (Maybe AwsEnvAuth)
getSTSWithRole rawAwsEnv region lgr p sourceProfile mfa (RoleArn arn) = do
  logDebug "Getting STS with role assume..."
  env <- runMaybeT $
      MaybeT (getFromSessionCache p sourceProfile mfa) <|>
      MaybeT (getSTS rawAwsEnv region lgr p sourceProfile mfa)
  sessionName <- liftIO $ getSessionName
  case env of
    Nothing -> liftIO $ error "Failed to get STS Credentials"
    Just (AwsEnvAuth k s sesst) -> do
      let auth = AWS.FromSession k s (fromJust sesst) -- TODO: fix this
      awsenv <- liftIO $ AWS.newEnv auth
      -- TODO: set duration
      let arn' = STS.assumeRole arn sessionName
      (creds, ars) <- liftIO $ parseAssumeRoleAuthEnvOrDie =<< execStsRequest awsenv lgr region =<< (mkStsAssumeRoleRequest mfa arn')
      cacheFileName <- liftIO $ mkAwsRoleFileName p (RoleArn arn)
      liftIO $ writeRoleCache cacheFileName (toRoleCache creds ars)
      let auth = AwsEnvAuth
                  (creds ^. AWS.accessKeyId)
                  (creds ^. AWS.secretAccessKey)
                  (creds ^. AWS.sessionToken)
      return $ Just auth


-- No CACHE, just use raw creds we already have
useRaw :: (HasLogFunc env, HasProcessContext env) => AWS.AccessKey -> AWS.SecretKey -> RIO env (Maybe AwsEnvAuth)
useRaw k s = do
  logDebug "Using raw credentials..."
  return $ Just $ AwsEnvAuth k s Nothing

-- Uses raw creds we aleady have but still we assume a role
-- Important: You cannot call AssumeRole by using AWS root account credentials;
-- access is denied. You must use credentials for an IAM user or an IAM role to call AssumeRole .
useRawWithRole :: (HasLogFunc env, HasProcessContext env) => AWS.Env -> AWS.Region -> AWS.Logger ->  AwsProfile -> Maybe AwsSourceProfile -> RoleArn -> RIO env (Maybe AwsEnvAuth)
useRawWithRole rawAwsEnv region lgr p sourceProfile (RoleArn arn) = do
  logDebug "Using raw credentials to assume role..."
  sessionName <- liftIO getSessionName
  -- TODO: set duration
  let arn' = STS.assumeRole arn sessionName
  (creds, ars) <- liftIO $ parseAssumeRoleAuthEnvOrDie =<< execStsRequest rawAwsEnv lgr region arn'
  cacheFileName <- liftIO $ mkAwsRoleFileName p (RoleArn arn)
  liftIO $ writeRoleCache cacheFileName (toRoleCache creds ars)
  let auth = AwsEnvAuth
              (creds ^. AWS.accessKeyId)
              (creds ^. AWS.secretAccessKey)
              (creds ^. AWS.sessionToken)
  return $ Just auth


nop :: (HasLogFunc env, HasProcessContext env) => RIO env (Maybe AwsEnvAuth)
nop = return Nothing

nop1 :: (HasLogFunc env, HasProcessContext env) => a -> RIO env (Maybe AwsEnvAuth)
nop1 _ = return Nothing


mkAwsEnv :: AwsProfile -> [String] -> Bool -> IO ()
mkAwsEnv p cmd debug = do
    when (null cmd) checkEnv
    lo <- logOptionsHandle stderr debug
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf -> do
      let awsEnv = AwsEnv {awsEnvLogger = lf, awsEnvProcessContext = pc}
      config <- parseConfigOrDie
      creds <- parseCredentialsOrDie
      let sourceProfile = eitherToMaybe $ getSourceProfile p config
          key = eitherToMaybe $ getAwsAccessKey sourceProfile p creds
          secret = eitherToMaybe $ getSecretAccessKey sourceProfile p creds
          region = getRegion sourceProfile p config
          mfaSerial = eitherToMaybe $ getMfaSerial sourceProfile p config
          roleArn = eitherToMaybe $ getRoleArn sourceProfile p config
      when (isNothing key) $ die "ERROR: Failed to parse aws_access_key_id from credentials file."
      when (isNothing secret) $ die "ERROR: Failed to parse aws_secret_access_key from credentials file."
      let key' = fromJust key
      let secret' = fromJust secret
      lgr <- AWS.newLogger AWS.Info stdout
      let auth = AWS.FromKeys key' secret'
      awsenv <- AWS.newEnv auth
      let region' :: AWS.Region =
            either (const AWS.NorthVirginia) id $ fromText =<< region
        -- either
        --   (\err -> die $ "ERROR Failed to parse credentials: " <> err)
        --   AWS.newEnv
        --   auth
      env <- runMaybeT $
        MaybeT (runRIO awsEnv $ maybe nop (getFromRoleCache p) roleArn) <|>
        MaybeT (runRIO awsEnv $ maybe nop (maybe nop1 (getSTSWithRole awsenv region' lgr p sourceProfile) mfaSerial) roleArn) <|>
        MaybeT (runRIO awsEnv $ maybe nop (getFromSessionCache p sourceProfile) mfaSerial) <|>
        MaybeT (runRIO awsEnv $ maybe nop (getSTS awsenv region' lgr p sourceProfile) mfaSerial) <|>
        MaybeT (runRIO awsEnv $ maybe nop (useRawWithRole awsenv region' lgr p sourceProfile) roleArn) <|>
        MaybeT (runRIO awsEnv $ useRaw key' secret')
      case env of
        Nothing -> die "Something went horribly wrong." -- TODO: Don't error like this
        Just (AwsEnvAuth k s sesst) -> do
          case cmd of
            [] -> writeRc $ mkAwsEnv' p region' (AwsEnvAuth k s sesst)
            (x:xs) -> do
                let env = Map.fromList [ (show AwsAccessKeyId, T.unpack $ toText k)
                                       , (show AwsSecretAccessKey, T.unpack $ toText s)
                                       , (show AwsDefaultRegion, T.unpack $ toText region')
                                       , (show AwsRegion, T.unpack $ toText region')
                                       ]
                let env' = maybe env (\t ->
                              Map.insert (show AwsSecurityToken) (T.unpack $ toText t) $
                              Map.insert (show AwsSessionToken) (T.unpack $ toText t) env) sesst
                parentEnv <- getEnvironment
                TP.runProcess_ $ TP.setEnv (Map.toList $ Map.union env' $ Map.fromList parentEnv) (TP.shell $ intercalate " " (x:xs))


mkAwsEnv' ::
     AwsProfile
  -> AWS.Region
  -> AwsEnvAuth
  -> [DenvVariable]
mkAwsEnv' p region (AwsEnvAuth k s sesst)=
  withVarTracking
    Nothing
    [ Set AwsAccessKeyId $ toText k
    , Set AwsSecretAccessKey $ toText s
    , maybe (Unset AwsSessionToken) (\x -> Set AwsSessionToken $ toText x) sesst
    , Set AwsSecurityToken $ mkEscapedText "$AWS_SESSION_TOKEN"
    , Set AwsDefaultRegion (toText region)
    , Set AwsRegion (toText region)
    , Set OldPrompt ps1
    , Set DenvPrompt $ mkEscapedText $ "aws|" <> fromAwsProfile p <> " "
    , Set Prompt $ "$_DENV_PROMPT$PS1"
    ]

execStsRequest :: AWS.AWSRequest a =>
     AWS.Env -> AWS.Logger -> AWS.Region ->  a -> IO (AWS.Rs a)
execStsRequest awsenv lgr region req = do
  ret <-
    runResourceT $
    AWS.runAWS (awsenv & AWS.envLogger .~ lgr) $
    AWS.within region $ AWS.send req
  return ret

parseAssumeRoleAuthEnvOrDie :: STS.AssumeRoleResponse -> IO (AWS.AuthEnv, STS.AssumedRoleUser)
parseAssumeRoleAuthEnvOrDie ret = do
  let creds = ret ^. STS.arrsCredentials
  let ars = ret ^. STS.arrsAssumedRoleUser
  let status = ret ^. STS.arrsResponseStatus
  case status of
    x
      | x /= 200 -> die $ "ERROR: STS error: " <> show ret
    200 -> return $ (fromJust creds, fromJust ars) -- NOTE: This should be OK since it's a 200 response

parseSessionTokenAuthEnvOrDie :: STS.GetSessionTokenResponse -> IO (AWS.AuthEnv)
parseSessionTokenAuthEnvOrDie ret = do
  let got = ret ^. STS.gstrsCredentials
  let status = ret ^. STS.gstrsResponseStatus
  case status of
    x
      | x /= 200 -> die $ "ERROR: STS error: " <> show ret
    200 -> return $ fromJust got -- NOTE: This should be ok since it's a 200 response

mkStsSessionTokenRequest :: MfaSerial -> STS.GetSessionToken -> IO STS.GetSessionToken
mkStsSessionTokenRequest (MfaSerial mfaSerial) st = do
  req <- do
        tokenCode <-
          promptLine $
          "Enter MFA code for device " <> show mfaSerial <> ": "
        return $
          st & STS.gstTokenCode .~ (Just $ T.pack tokenCode) &
          STS.gstSerialNumber .~ (Just mfaSerial) &
          STS.gstDurationSeconds .~ (Just $ fromIntegral defaultSessionDurationSeconds)
  return req

-- We're currently not supporting assuming role with raw credentials with MFA.
-- If MFA is present we will always favor session credentials which will be used for
-- requesting the role credentials
mkStsAssumeRoleRequest :: MfaSerial -> STS.AssumeRole -> IO STS.AssumeRole
mkStsAssumeRoleRequest (MfaSerial mfaSerial) ar = do
  return ar
  -- req <- do
  --       tokenCode <-
  --         promptLine $
  --         "Enter MFA code for " <> (T.unpack $ ar ^. STS.arRoleARN) <> ": "
  --       return $
  --         ar & STS.arTokenCode .~ (Just $ T.pack tokenCode) &
  --         STS.arSerialNumber .~ (Just mfaSerial)
  -- return req

writeRoleCache :: FilePath -> AwsEnvRoleCache -> IO ()
writeRoleCache fp cache = do
  TLIO.writeFile fp (encodeToLazyText cache)
  setFileMode fp $ unionFileModes ownerReadMode ownerWriteMode

writeSessionCache :: FilePath -> AwsEnvSessionCache -> IO ()
writeSessionCache fp cache = do
  TLIO.writeFile fp (encodeToLazyText cache)
  setFileMode fp $ unionFileModes ownerReadMode ownerWriteMode
