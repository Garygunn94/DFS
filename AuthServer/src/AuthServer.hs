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

data Response = Response{
  response :: String
} deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

data User = User{
    uusername :: String,
    upassword :: String,
	timeout :: String,
	token :: String
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
    "isvalid" :> ReqBody '[JSON] User :> Post '[JSON] Response

authApi :: Proxy AuthApi
authApi = Proxy

server :: Server AuthApi
server = 
    login :<|>
    newuser :<|>
    checkToken

authApp :: Application
authApp = serve authApi server

mkApp :: IO()
mkApp = do
    run (read (serverport) ::Int) authApp 

login :: Signin -> ApiHandler User
login signin = liftIO $ do
	let uname = susername signin
	let psswrd = spassword signin
	warnLog $ "Searching for value for key: " ++ uname ++ ", " ++ psswrd
	user <- withMongoDbConnection $ do 
	    docs <- find (select ["susername" =: uname] "USER_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Signin) docs
        let thisuser = head $ user
        putStrLn (show thisuser)
        a <- nextUUID
        let tokener = Data.UUID.toString $ fromJust a
        currentTime <- getCurrentTime
        currentZone <- getCurrentTimeZone
        let fiveMinutes = 5 * 60
        let newTime = addUTCTime fiveMinutes currentTime
        let timeouter = utcToLocalTime currentZone newTime
        let finaltimeout = show timeouter
        warnLog $ "Storing Session key under key " ++ uname ++ "."
        let session = (User (susername thisuser) (spassword thisuser) finaltimeout tokener)
	withMongoDbConnection $ upsert (select ["uusername" =: uname] "SESSION_RECORD") $ toBSON session
        warnLog $ "Session Successfully Stored."
	return (session)


newuser :: Signin -> ApiHandler Response
newuser signin = liftIO $ do
  let uname = susername signin
  let psswrd = spassword signin
  warnLog $ "Storing value under key: " ++ uname
  let user = (Signin uname psswrd)
  withMongoDbConnection $ upsert (select ["susername" =: uname] "USER_RECORD") $ toBSON user
  return (Response "Success")

checkToken :: User -> ApiHandler Response
checkToken user = liftIO $ do
  let uname = uusername user
  let tokener = token user
  warnLog $ "Searching for user: " ++ uname
  session <- withMongoDbConnection $ do 
      docs <- find (select ["uusername" =: uname] "SESSION_RECORD") >>= drainCursor
      return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe User) docs
  warnLog $ "User found" ++ (show session)
  let thissession = head $ session
  let senttoken = token user
  let storedtoken = token thissession
  case senttoken == storedtoken of
      False -> return (Response "Token not valid")
      True -> do let tokentimeout = timeout thissession
                 currentTime <- getCurrentTime
                 currentZone <- getCurrentTimeZone
                 let localcurrentTime = show (utcToLocalTime currentZone currentTime)
                 let tokentimeoutsplit = words $ tokentimeout
                 let localcurrentTimesplit = words $ localcurrentTime
                 case ((tokentimeoutsplit !! 0) == (localcurrentTimesplit !! 0)) of
                      False -> return (Response "Current date not equal to token date")
                      True -> do let localcurrenthours = localcurrentTimesplit !! 1
                                 let tokenhours = tokentimeoutsplit !! 1
                                 let localhour = (splitOn ":" $ localcurrenthours) !! 0
                                 let tokenhour = (splitOn ":" $ tokenhours) !! 0
                                 case localhour == tokenhour of
                                   False -> return (Response "Either token timeout or hour rollover")
                                   True -> do let tokenminutes = read(((splitOn ":" $ tokenhours) !! 1))
                                              let currentminutes = read(((splitOn ":" $ localcurrenthours) !! 1))
                                              case ((tokenminutes - currentminutes)<= 5) of
                                                  False -> return (Response "Token Timeout")
                                                  True -> return (Response "Token is Valid")







  










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
