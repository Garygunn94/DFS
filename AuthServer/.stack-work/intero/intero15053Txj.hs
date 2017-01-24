{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AuthServer where
import System.Random
import           Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.List.Split
import           Crypto.BCrypt
import           Crypto.Cipher.AES
import Crypto.Random.DRBG
import Codec.Crypto.RSA
import           GHC.Generics
import           Network.Wai hiding(Response)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           System.Directory
import           	System.Environment           (getArgs, getProgName, lookupEnv)
import           	System.Log.Formatter
import           	System.Log.Handler           (setFormatter)
import           	System.Log.Handler.Simple
import           	System.Log.Handler.Syslog
import           	System.Log.Logger
import         	  Data.Bson.Generic
import qualified	Data.List                    as DL
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8 as C
import           	Data.Maybe                   (catMaybes, fromJust)
import           	Data.Text                    (pack, unpack)
import           	Data.Time.Clock              (UTCTime, getCurrentTime)
import           	Data.Time.Format             (defaultTimeLocale, formatTime)
import           	Database.MongoDB 
import Control.Monad (when)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.UUID.V1
import Data.UUID hiding (null)
import Data.Time
import Crypto.Types.PubKey.RSA
import OpenSSL.EVP.PKey

data Response = Response{
  response :: String
} deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

data KeyMapping = KeyMapping{
  auth :: String,
  publickey :: String,
  privatekey :: String
  }deriving (Eq, Show, Generic)
instance ToJSON KeyMapping
instance FromJSON KeyMapping
instance ToBSON KeyMapping
instance FromBSON KeyMapping

data User = User{
    uusername :: String,
    upassword :: String,
	timeout :: String,
	utoken :: String
} deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User
instance ToBSON User
instance FromBSON User

data Signin = Signin{
	susername :: String,
	spassword :: String
} deriving (Eq, Show, Generic)
instance ToJSON Signin
instance FromJSON Signin
instance ToBSON Signin
instance FromBSON Signin

type ApiHandler = ExceptT ServantErr IO

serverport :: String
serverport = "8082"

serverhost :: String
serverhost = "localhost"

type AuthApi = 
    "signin" :> ReqBody '[JSON] Signin :> Post '[JSON] User :<|>
    "register" :> ReqBody '[JSON] Signin :> Post '[JSON] Response  :<|>
    "isvalid" :> ReqBody '[JSON] User :> Post '[JSON] Response :<|>
    "extend" :> ReqBody '[JSON] User :> Post '[JSON] Response

authApi :: Proxy AuthApi
authApi = Proxy

server :: Server AuthApi
server = 
    login :<|>
    newuser :<|>
    checkToken :<|>
    extendToken

authApp :: Application
authApp = serve authApi server

mkApp :: IO()
mkApp = do
    run (read (serverport) ::Int) authApp 

login :: Signin -> ApiHandler User
login signin@(Signin uName psswrd) = liftIO $ do
	decryptedpassword <- decryptPassword psswrd
	warnLog $ "Decrypted password: " ++ decryptedpassword ++ ", For user: " ++ uName
	user <- withMongoDbConnection $ do 
	    docs <- find (select ["_id" =: uName] "USER_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Signin) docs
        let thisuser = head $ user
        let isvalid = checkPassword thisuser decryptedpassword
        case isvalid of
          False -> return (User "" "" "" "")
          True -> do a <- nextUUID
                     let sessionKey = Data.UUID.toString $ fromJust a
                     currentTime <- getCurrentTime
                     currentZone <- getCurrentTimeZone
                     let fiveMinutes = 5 * 60
                     let newTime = addUTCTime fiveMinutes currentTime
                     let timeouter = utcToLocalTime currentZone newTime
                     let finaltimeout = show timeouter
                     warnLog $ "Storing Session key under key " ++ uName ++ "."
                     let session = (User (susername thisuser) "" finaltimeout sessionKey)
            	     withMongoDbConnection $ upsert (select ["_id" =: uName] "SESSION_RECORD") $ toBSON session
                     warnLog $ "Session Successfully Stored."
            	     return (session)


newuser :: Signin -> ApiHandler Response
newuser v@(Signin uName psswrd) = liftIO $ do
  warnLog $ "Registering account: " ++ uName
  warnLog $ "Encrypted password: " ++ psswrd
  unencrypted <- decryptPassword psswrd
  warnLog $ "Decrypted password: " ++ unencrypted
  withMongoDbConnection $ do
    docs <- findOne (select ["_id" =: uName] "USER_RECORD")
    case docs of
      Just _ -> return (Response "Account already exists")
      Nothing -> liftIO $ do
        hash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack unencrypted)
        let hashedpsswrd = BS.unpack $ fromJust hash
        liftIO $ do
          warnLog $ "Storing new user: " ++ uName ++ ". With hashed password: " ++ hashedpsswrd
          let newaccount = (Signin uName hashedpsswrd)
          withMongoDbConnection $ upsert (select ["_id" =: uName] "USER_RECORD") $ toBSON newaccount
          return (Response "Success")

checkToken :: User -> ApiHandler Response
checkToken user = liftIO $ do
  let uname = uusername user
  let tokener = utoken user
  warnLog $ "Searching for user: " ++ uname
  session <- withMongoDbConnection $ do 
      docs <- find (select ["_id" =: uname] "SESSION_RECORD") >>= drainCursor
      return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe User) docs
  warnLog $ "User found" ++ (show session)
  let thissession = head $ session
  let senttoken = utoken user
  let storedtoken = utoken thissession
  case senttoken == storedtoken of
      False -> return (Response "Token not valid")
      True -> do let tokentimeout = timeout thissession
                 currentTime <- getCurrentTime
                 currentZone <- getCurrentTimeZone
                 let localcurrentTime = show (utcToLocalTime currentZone currentTime)
                 let tokentimeoutsplit = splitOn " " $ tokentimeout
                 let localcurrentTimesplit = splitOn " " $ localcurrentTime
                 case ((tokentimeoutsplit !! 0) == (localcurrentTimesplit !! 0)) of
                      False -> return (Response "Current date not equal to token date")
                      True -> do let localcurrenthours = localcurrentTimesplit !! 1
                                 let tokenhours = tokentimeoutsplit !! 1
                                 let localhour = read(((splitOn ":" $ localcurrenthours) !! 0))
                                 let tokenhour = read(((splitOn ":" $ tokenhours) !! 0))
                                 let tokenminutes = read(((splitOn ":" $ tokenhours) !! 1))
                                 let currentminutes = read(((splitOn ":" $ localcurrenthours) !! 1))
                                 let totaltokenminutes = (tokenhour * 60) + tokenminutes
                                 let totalcurrentminutes = (localhour * 60) + currentminutes
                                 case totaltokenminutes > totalcurrentminutes of
                                      False -> return (Response "Token Timeout")
                                      True -> return (Response "Token is Valid")


extendToken :: User -> ApiHandler Response
extendToken user = liftIO $ do
  currentTime <- getCurrentTime
  currentZone <- getCurrentTimeZone
  let fiveMinutes = 5 * 60
  let newTime = addUTCTime fiveMinutes currentTime
  let timeouter = utcToLocalTime currentZone newTime
  let finaltimeout = show timeouter
  warnLog $ "Storing Session key under key " ++ (uusername user) ++ "."
  let session = (User (uusername user) "" finaltimeout (utoken user))
  withMongoDbConnection $ upsert (select ["_id" =: (uusername user)] "SESSION_RECORD") $ toBSON session
  warnLog $ "Session Successfully Stored."
  return (Response "Success")


loadOrGenPublicKey :: ApiHandler Response
loadOrGenPublicKey = liftIO $ do
  withMongoDbConnection $ do
    let auth= "auth" :: String
    docs <- find (select ["_id" =: auth] "KEY_RECORD") >>= drainCursor
    let key = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe KeyMapping) docs
    case key of
      [(KeyMapping _ publickey privatekey)]-> return (Response publickey)
      [] -> liftIO $ do
        r1 <- newGenIO :: IO HashDRBG
        let (publickey,privatekey,g2) = generateKeyPair r1 1024
        --let strPublicKey = fromPublicKey publickey
        --let strPublicKey = (PublicKeyInfo (show(key_size publickey)) (show(n publickey)) (show(e publickey)))
        --let strPrivateKey = fromPrivateKey privatekey
        --let strPrivateKey = (PrivateKeyInfo (PublicKeyInfo (show(key_size (private_pub privatekey)))) (show(n (private_pub privatekey))) (show(e (private_pub privatekey))))
        let key = (KeyMapping auth (show publickey) (show privatekey))
        withMongoDbConnection $ upsert (select  ["_id" =: auth] "KEY_RECORD") $ toBSON key
        return (Response (show publickey))

decryptPassword :: String ->  IO String
decryptPassword psswrd = do
    withMongoDbConnection $ do
      let auth= "auth" :: String
      keypair <- find (select ["_id" =: auth] "KEY_RECORD") >>= drainCursor
      let [(KeyMapping _ publickey privatekey)]= catMaybes $ DL.map (\ b -> fromBSON b :: Maybe KeyMapping) keypair
      let prvKey = toPrivateKey privatekey
      let password = C.pack psswrd
      let decryptedpassword = decrypt privateKey password
      return $ C.unpack decryptedpassword

checkPassword :: Signin -> String -> Bool
checkPassword val@(Signin _ hash) password = validatePassword (BS.pack hash) (BS.pack password)

  










 -- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger

-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  Database.MongoDB.close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" Prelude.id "127.0.0.1" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" Prelude.id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" Prelude.id "DEBUG" True

-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def
