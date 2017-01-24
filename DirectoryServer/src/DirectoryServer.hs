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
import            CommonResources
import MongodbHelpers

--manager = newManager defaultManagerSettings

type ApiHandler = ExceptT ServantErr IO


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

uploadQuery :: File -> ClientM Response
uploadQuery file = do
        get_upload <- upload file
        return (get_upload)

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

server :: Server DirectoryApi
server = 
    fsJoin :<|>
    DirectoryServer.openFile :<|>
    closeFile :<|>
    allFiles

directoryApp :: Application
directoryApp = serve directoryApi server

mkApp :: IO()
mkApp = do
    run (read (dirserverport) ::Int) directoryApp 

deriving instance FromBSON FileServer  -- we need these as BSON does not provide
deriving instance ToBSON   FileServer

storefs:: FileServer -> IO()
storefs fs@(FileServer key _ _) = liftIO $ do
    warnLog $ "Storing file under key " ++ key ++ "."
    withMongoDbConnection $ upsert (select ["_id" =: key] "FILESERVER_RECORD") $ toBSON fs
    warnLog $ "Success"
   -- return True

storefm :: String  -> String-> [FileMapping] -> String -> IO[FileMapping]
storefm  port address a  filename =  do
	warnLog $ "Storing file under key " ++ filename ++ "."
        let fileMapping = (FileMapping filename address port)
	withMongoDbConnection $ upsert (select ["_id" =: filename] "FILEMAPPING_RECORD") $ toBSON fileMapping
        return $ (FileMapping filename address port):a
--	return True

getStoreFm :: FileServer -> IO()
getStoreFm fs = liftIO $ do
    manager <- newManager defaultManagerSettings
    res <- runClientM getFilesQuery (ClientEnv manager (BaseUrl Http (fsaddress fs) (read(fsport fs) :: Int) ""))
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right response' -> do
          blah' <- mapM (storefm (fsport fs) (fsaddress fs)[]) response'
          return ()
                         
                         
   -- return True

fsJoin :: FileServer -> ApiHandler Response
fsJoin fs = liftIO $  do
	storefs fs

	return (Response "Success")

searchFileMappings :: String -> IO(FileMapping)
searchFileMappings key =  do
	warnLog $ "Searching for value for key: " ++ key
	filemappings <- withMongoDbConnection $ do 
	    docs <- find (select ["_id" =: key] "FILEMAPPING_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
        return $ head $ filemappings

openFileQuery :: String -> FileMapping -> IO(File)
openFileQuery key fm =  do
	manager <- newManager defaultManagerSettings
        res <- runClientM (downloadQuery key) (ClientEnv manager (BaseUrl Http (fmaddress fm) (read(fmport fm) :: Int) ""))
        case res of
           Left err -> return (File "" "")
                          
           Right response -> return (response) 

openFile :: String -> ApiHandler File
openFile key = liftIO $ do
          fileServers <- liftIO $ withMongoDbConnection $ do
              docs <- find (select [] "FILESERVER_RECORD") >>= drainCursor
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServer) docs
          mapM getStoreFm fileServers
	  fm <- searchFileMappings key
	  file <- openFileQuery key fm
          return file

closeFile :: File -> ApiHandler Response
closeFile file = liftIO $  do
  fileServers <- liftIO $ withMongoDbConnection $ do
       docs <- find (select [] "FILESERVER_RECORD") >>= drainCursor
       return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServer) docs
  let range = length fileServers
  index <- randomRIO (0, (range-1))
  let fs = fileServers !! index
  let fm = (FileMapping (fileName file) (fsaddress fs) (fsport fs))
  withMongoDbConnection $ upsert (select ["_id" =: (fileName file)] "FILEMAPPING_RECORD") $ toBSON fm
  manager <- newManager defaultManagerSettings
  res <- runClientM (uploadQuery file) (ClientEnv manager (BaseUrl Http (fsaddress fs) (read(fsport fs) :: Int) ""))
  case res of
   Left err -> return (Response "Failed")
                          
   Right response -> return (response)
  
  
allFiles :: ApiHandler [String]
allFiles = liftIO $ do
          fileServers <- liftIO $ withMongoDbConnection $ do
              docs <- find (select [] "FILESERVER_RECORD") >>= drainCursor
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServer) docs
          mapM getStoreFm fileServers
          filemappings <- liftIO $ withMongoDbConnection $ do
              docs <- find (select [] "FILEMAPPING_RECORD") >>= drainCursor
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
          let filenames = map fmfileName filemappings
          return filenames
  
            



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