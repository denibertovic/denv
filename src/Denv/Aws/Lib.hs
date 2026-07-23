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
parseRoleCacheFile :: AwsProfile -> Natural -> RoleArn -> IO (Either AwsEnvCacheError AwsEnvRoleCache)
parseRoleCacheFile p duration (RoleArn arn') = do
    fileName <- mkAwsRoleFileName p (RoleArn arn')
    exists <- doesFileExist fileName
    if (not exists) then (return $ Left AwsEnvCacheNotFound) else do
      c <- BSL.readFile fileName
      let c' = eitherDecode c :: Either _ AwsEnvRoleCache
      case c' of
        Left _ -> return $ Left AwsEnvCacheParseError
        Right r' -> do
          let exp = fromJust $ roleCacheExpiration r' -- TODO: fix this
          isExpired <- isRoleCacheExpired exp duration
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

durationToSeconds :: AwsRoleSessionDuration -> Integer
durationToSeconds d = case d of
  Duration_1h -> 3600
  Duration_2h -> 7200
  Duration_3h -> 10800
  Duration_4h -> 14400
  Duration_5h -> 18000
  Duration_6h -> 21600
  Duration_7h -> 25200
  Duration_8h -> 28800
  Duration_9h -> 32400
  Duration_10h -> 36000
  Duration_11h -> 39600
  Duration_12h -> 43200

-- Default session_refresh_factor is 65
isSessionCacheExpired exp = do
  now <- getCurrentTime
  let now' = systemSeconds $ utcToSystemTime now
  let exp' = (systemSeconds $ utcToSystemTime exp) - ((fromIntegral defaultSessionDurationSeconds) * defaultSessionRefreshFactor `div` 100)
  if (now' >= exp') then return True else return False

-- Default role_refresh_factor is 35
isRoleCacheExpired exp duration = do
  now <- getCurrentTime
  let now' = systemSeconds $ utcToSystemTime now
  let exp' = (systemSeconds $ utcToSystemTime exp) - fromIntegral (duration * defaultRoleRefreshFactor `div` 100)
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

getFromRoleCache :: (HasLogFunc env, HasProcessContext env) => AwsProfile -> Natural -> RoleArn -> RIO env (Maybe AwsEnvAuth)
getFromRoleCache p duration r = do
  -- parsing the file does the expiration check
  -- TODO: separate the expiration check into a separate function to
  -- make it more clear
  r <- liftIO $ parseRoleCacheFile p duration r
  case r of
    Left err -> do
      return Nothing
    Right r' -> do
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

-- Assume a role using the given AWS env, caching and returning the resulting
-- credentials. When an MFA serial is supplied the user is prompted for a token
-- code -- this is required when assuming a role directly with long-term IAM
-- credentials. Callers that already hold MFA-backed session credentials pass
-- Nothing, since those credentials already satisfy the MFA requirement.
assumeRoleWith :: (HasLogFunc env, HasProcessContext env) => AWS.Env -> AWS.Region -> AWS.Logger -> AwsProfile -> Maybe MfaSerial -> Natural -> RoleArn -> RIO env AwsEnvAuth
assumeRoleWith awsenv region lgr p mfa duration (RoleArn arn) = do
  sessionName <- liftIO getSessionName
  let ar = STS.assumeRole arn sessionName & STS.arDurationSeconds .~ Just duration
  ar' <- liftIO $ maybe (return ar) (\m -> mkStsAssumeRoleRequest m ar) mfa
  (creds, ars) <- liftIO $ parseAssumeRoleAuthEnvOrDie =<< execStsRequest awsenv lgr region ar'
  cacheFileName <- liftIO $ mkAwsRoleFileName p (RoleArn arn)
  liftIO $ writeRoleCache cacheFileName (toRoleCache creds ars)
  return $ AwsEnvAuth
             (creds ^. AWS.accessKeyId)
             (creds ^. AWS.secretAccessKey)
             (creds ^. AWS.sessionToken)

-- Use Session Credentials to make an AssumeRole call
-- Only use "Raw" (user) credentials for getting the STS credentials and then use those
-- to assume the role
getSTSWithRole :: (HasLogFunc env, HasProcessContext env) => AWS.Env -> AWS.Region -> AWS.Logger -> AwsProfile -> Maybe AwsSourceProfile -> Natural -> MfaSerial -> RoleArn -> RIO env (Maybe AwsEnvAuth)
getSTSWithRole rawAwsEnv region lgr p sourceProfile duration mfa roleArn = do
  env <- if duration <= (fromIntegral $ durationToSeconds Duration_1h) then do
    logDebug "Getting STS with role assume..."
    runMaybeT $
        MaybeT (getFromSessionCache p sourceProfile mfa) <|>
        MaybeT (getSTS rawAwsEnv region lgr p sourceProfile mfa)
  else do
    logDebug "Duration is longer than 1h. Skipping GetSessionToken API."
    return Nothing

  case env of
    -- No session token available (either skipped for a >1h duration or the
    -- GetSessionToken call failed): assume the role directly with the long-term
    -- credentials, which requires prompting for an MFA token code.
    Nothing -> do
      logDebug "Using RAW creds with role assume..."
      Just <$> assumeRoleWith rawAwsEnv region lgr p (Just mfa) duration roleArn
    -- We already hold MFA-backed session credentials; chain off them to assume
    -- the role. AWS caps chained-role sessions at 1h, so no extra MFA prompt.
    Just (AwsEnvAuth k s sesst) -> do
      logDebug "Using session creds with role assume..."
      awsenv <- liftIO $ AWS.newEnv (AWS.FromSession k s (fromJust sesst)) -- TODO: fix this
      Just <$> assumeRoleWith awsenv region lgr p Nothing duration roleArn


-- No CACHE, just use raw creds we already have
useRaw :: (HasLogFunc env, HasProcessContext env) => AWS.AccessKey -> AWS.SecretKey -> RIO env (Maybe AwsEnvAuth)
useRaw k s = do
  logDebug "Using raw credentials..."
  return $ Just $ AwsEnvAuth k s Nothing

-- Uses raw creds we aleady have but still we assume a role
-- Important: You cannot call AssumeRole by using AWS root account credentials;
-- access is denied. You must use credentials for an IAM user or an IAM role to call AssumeRole .
useRawWithRole :: (HasLogFunc env, HasProcessContext env) => AWS.Env -> AWS.Region -> AWS.Logger ->  AwsProfile -> Maybe AwsSourceProfile -> Natural -> RoleArn -> RIO env (Maybe AwsEnvAuth)
useRawWithRole rawAwsEnv region lgr p sourceProfile duration roleArn = do
  logDebug "Using raw credentials to assume role..."
  Just <$> assumeRoleWith rawAwsEnv region lgr p Nothing duration roleArn


-- Lift an optional value into MaybeT: a missing input short-circuits the branch
-- so the <|> chain falls through to the next credential source.
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure


mkAwsEnv :: AwsProfile -> Maybe AwsRoleSessionDuration -> [String] -> Bool -> IO ()
mkAwsEnv p duration cmd debug = do
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
      let duration' = maybe (fromIntegral defaultRoleDurationSeconds) (\d -> fromIntegral $ durationToSeconds d) duration
      awsenv <- AWS.newEnv auth
      let region' :: AWS.Region =
            either (const AWS.NorthVirginia) id $ fromText =<< region
        -- either
        --   (\err -> die $ "ERROR Failed to parse credentials: " <> err)
        --   AWS.newEnv
        --   auth
      env <- runRIO awsEnv $ runMaybeT $
            (hoistMaybe roleArn   >>= MaybeT . getFromRoleCache p duration')
        <|> (do arn <- hoistMaybe roleArn
                mfa <- hoistMaybe mfaSerial
                MaybeT $ getSTSWithRole awsenv region' lgr p sourceProfile duration' mfa arn)
        <|> (hoistMaybe mfaSerial >>= MaybeT . getFromSessionCache p sourceProfile)
        <|> (hoistMaybe mfaSerial >>= MaybeT . getSTS awsenv region' lgr p sourceProfile)
        <|> (hoistMaybe roleArn   >>= MaybeT . useRawWithRole awsenv region' lgr p sourceProfile duration')
        <|>  MaybeT (useRaw key' secret')
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
  req <- do
        tokenCode <-
          promptLine $
          "Enter MFA code for device " <> show mfaSerial <> ": "
        return $
          ar & STS.arTokenCode .~ (Just $ T.pack tokenCode) &
          STS.arSerialNumber .~ (Just mfaSerial)
  return req

writeRoleCache :: FilePath -> AwsEnvRoleCache -> IO ()
writeRoleCache fp cache = do
  TLIO.writeFile fp (encodeToLazyText cache)
  setFileMode fp $ unionFileModes ownerReadMode ownerWriteMode

writeSessionCache :: FilePath -> AwsEnvSessionCache -> IO ()
writeSessionCache fp cache = do
  TLIO.writeFile fp (encodeToLazyText cache)
  setFileMode fp $ unionFileModes ownerReadMode ownerWriteMode
