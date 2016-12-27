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

module DirectoryServer where

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

manager = newManager defaultManagerSettings

data File = File { 
    fileName :: FilePath, 
    fileContent :: String 
} deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

data Response = Response{
  response :: String
} deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

data FileServer = FileServer{
    id :: String,
	fsaddress :: String,
	fsport :: String
} deriving (Eq, Show, Generic)

instance ToJSON FileServer
instance FromJSON FileServer
instance ToBSON FileServer
instance FromBSON FileServer

data FileMapping = FileMapping{
	fmfileName :: String,
	fmaddress :: String,
	fmport :: String
} deriving (Eq, Show, Generic)

instance ToJSON FileServer
instance FromJSON FileServer
instance ToBSON FileServer
instance FromBSON FileServer

type ApiHandler = ExceptT ServantErr IO

serverport :: String
serverport = "7008"

serverhost :: String
serverhost = "localhost"

type DirectoryApi = 
    "join" :> ReqBody '[JSON] FileServer :> Post '[JSON] Response :<|>
    "open" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "close" :> ReqBody '[JSON] File :> Post '[JSON] Response

type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "upload" :> ReqBody '[JSON] File :> Post '[JSON] Response -- :<|>

fileApi :: Proxy FileApi
fileApi = Proxy

files:: ClientM [FilePath]
download :: String -> ClientM File
upload :: File -> ClientM Response

files :<|> download :<|> upload = client fileApi


getFilesQuery :: ClientM[FilePath]
getFilesQuery = do
	get_files <- files
	return(get_files)

downloadQuery :: String -> ClientM File
downloadQuery fname = do
	get_download <- download (fname)
	return(get_download)

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

server :: Server DirectoryApi
server = 
    fsJoin :<|>
    DirectoryServer.openFile :<|>
    closeFile

directoryApp :: Application
directoryApp = serve directoryApi server

mkApp :: IO()
mkApp = do
    run (read (serverport) ::Int) directoryApp 

storefs:: FileServer -> Bool
storefs fs@(FileServer key _ _) = liftIO $ do
    warnLog $ "Storing file under key " ++ key ++ "."
	withMongoDbConnection $ upsert (select ["id" =: key] "FILESERVER_RECORD") $ toBSON fs
	return True

storefm :: FileMapping -> Bool
storefm fm@(FileMapping key _ _) = liftIO $ do
	warnLog $ "Storing file under key " ++ key ++ "."
	withMongoDbConnection $ upsert (select ["id" =: key] "FILEMAPPING_RECORD") $ toBSON fm
	return True
getStoreFm :: FileServer -> Bool
getStoreFm fs = do
    manager <- newManager defaultManagerSettings
    res <- runClientM getFilesQuery (ClientEnv manager (BaseUrl Http (fsaddress fs) (read(fsport fs)) ""))
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right response -> map (storefm (fsaddress fs) (fsport fs)) response
    return True

fsJoin :: FileServer -> ApiHandler Response
fsJoin fs = do
	bool <- storefs fs
	bool2 <- getStoreFm fs
	return (Response "Success")

searchFileMappings :: String -> Maybe FileMapping
searchFileMappings key = liftIO $ do
	warnLog $ "Searching for value for key: " ++ key
	withMongoDbConnection $ do 
	    docs <- find (select ["fmfileName" =: key] "FILEMAPPING_RECORD") >>= drainCursor
            file <- head $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
            return file

openFileQuery :: String -> FileMapping -> File
openFileQuery key fm = do
	manager <- newManager defaultManagerSettings
        res <- runClientM (downloadQuery key) (ClientEnv manager (BaseUrl Http (fmaddress fm) (read(fmport fm)) ""))
        case res of
           Left err -> putStrLn $ "Error: " ++ show err
           Right response -> return response 

openFile :: String -> ApiHandler File
openFile key = do
	        fm <- searchFileMappings key
	        case fm of
	        	Nothing -> putStrLn $ "Error: " ++ "File not found"
	        	Just filemapping -> do
	        		file <- openFileQuery key filemapping
	        		return file
            



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
mongoDbIp = defEnv "MONGODB_IP" Prelude.id "database" True

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
