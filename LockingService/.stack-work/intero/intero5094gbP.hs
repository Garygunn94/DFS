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

module LockingService where
import System.Random
import           Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
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
import           	Data.Maybe                   (catMaybes)
import           	Data.Text                    (pack, unpack)
import           	Data.Time.Clock              (UTCTime, getCurrentTime)
import           	Data.Time.Format             (defaultTimeLocale, formatTime)
import           	Database.MongoDB 
import Control.Monad (when)
import Network.HTTP.Client (newManager, defaultManagerSettings)

serverport :: String
serverport = "8000"

serverhost :: String
serverhost = "localhost"

data Lock = Lock{
	fileName :: String,
	locked :: Bool
} deriving(Eq, Show, Generic)
instance ToBSON Lock
instance FromBSON Lock

type ApiHandler = ExceptT ServantErr IO

type LockingApi = 
    "lock" :> Capture "fileName" String :> Get '[JSON] Bool :<|>
    "unlock" :> Capture "fileName" String :> Get '[JSON] Bool :<|>
    "islocked" :> Capture "fileName" String :> Get '[JSON] Bool

lockingApi :: Proxy LockingApi
lockingApi = Proxy

server :: Server LockingApi
server = 
    lockFile :<|>
    unlockFile :<|>
    isFileLocked

lockingApp :: Application
lockingApp = serve lockingApi server

mkApp :: IO()
mkApp = do
    run (read (serverport) ::Int) lockingApp


lockFile :: String -> ApiHandler Bool
lockFile fName = liftIO $ do
	warnLog $ "Locking File" ++ fName
	locks <- withMongoDbConnection $ do
            docs <- find (select ["_id" =: fName] "LOCK_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case locks of
            [(Lock _ True)] -> return False
            [(Lock _ False)] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: fName] "LOCK_RECORD") $ toBSON $ (Lock fName True)
                                            return True
            [] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: fName] "LOCK_RECORD") $ toBSON $ (Lock fName True)
                              return True

unlockFile :: String -> ApiHandler Bool
unlockFile fName = liftIO $ do
	warnLog $ "Unlocking File" ++ fName
	locks <- liftIO $ withMongoDbConnection $ do
                docs <- find (select ["_id" =: fName] "LOCK_RECORD") >>= drainCursor
                return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case locks of
           [(Lock _ False)] -> return False
           [(Lock _ True)] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: fName] "LOCK_RECORD") $ toBSON $ (Lock fName False)
                                          return True
           [] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: fName] "LOCK_RECORD") $ toBSON $ (Lock fName False)
                             return True

isFileLocked :: String -> ApiHandler Bool
isFileLocked fName = liftIO $ do
	warnLog $ "Checking is file" ++ fName ++ "is locked"
	locks <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["_id" =: fName] "LOCK_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case locks of
           [(Lock _ True)] -> return True
           _ -> return False


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
