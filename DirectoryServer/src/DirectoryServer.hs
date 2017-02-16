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
import CommonResources
import MongodbHelpers

--manager = newManager defaultManagerSettings

type ApiHandler = ExceptT ServantErr IO


fileApi :: Proxy FileApi
fileApi = Proxy

files:: ClientM [FilePath]
download :: FileName -> ClientM File
upload :: FileUpload -> ClientM Response
delete :: FileName -> ClientM Response

files :<|> download :<|> upload :<|> delete = client fileApi


getFilesQuery :: ClientM[FilePath]
getFilesQuery = do
	get_files <- files
	return(get_files)

downloadQuery :: FileName -> ClientM File
downloadQuery fname = do
	get_download <- download (fname)
	return(get_download)

uploadQuery :: FileUpload -> ClientM Response
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
    allFiles :<|>
    removeFileName

directoryApp :: Application
directoryApp = serve directoryApi server

mkApp :: IO()
mkApp = do
    putStrLn ("Starting Directory Server on port: " ++ dirserverport)
    run (read (dirserverport) ::Int) directoryApp 

deriving instance FromBSON FileServer  -- we need these as BSON does not provide
deriving instance ToBSON   FileServer

storefs:: FileServer -> IO()
storefs fs@(FileServer key _ _) = liftIO $ do
    putStrLn ("Storing file under key " ++ key ++ ".")
    withMongoDbConnection $ upsert (select ["_id" =: key] "FILESERVER_RECORD") $ toBSON fs
    putStrLn ("Success")
   -- return True

storefm :: String  -> String-> [FileMapping] -> String -> IO[FileMapping]
storefm  port address a  filename =  do
	putStrLn ("Storing file under key " ++ filename ++ ".")
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
	putStrLn ("Searching for value for key: " ++ key)
	filemappings <- withMongoDbConnection $ do 
	    docs <- find (select ["_id" =: key] "FILEMAPPING_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
        return $ head $ filemappings

openFileQuery :: FileName -> FileMapping -> IO(File)
openFileQuery fN fm =  do
	manager <- newManager defaultManagerSettings
        res <- runClientM (downloadQuery fN) (ClientEnv manager (BaseUrl Http (fmaddress fm) (read(fmport fm) :: Int) ""))
        case res of
           Left err -> return (File "" "")
                          
           Right response -> return (response) 

openFile :: FileName -> ApiHandler File
openFile fileName@(FileName ticket encryptedTimeout encryptedFN) = liftIO $ do
          let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
          let sessionKey = encryptDecrypt sharedSecret ticket
          putStrLn encryptedFN
          let decryptedFN = encryptDecrypt sessionKey encryptedFN
          putStrLn decryptedFN

          putStrLn ("Checking client timeout")

          currentTime <- getCurrentTime
          if (currentTime > decryptedTimeout) then do
            putStrLn ("Clients token timed out")
            return (File encryptedFN (encryptDecrypt sessionKey "Failed"))
          else do
            fileServers <- liftIO $ withMongoDbConnection $ do
              docs <- find (select [] "FILESERVER_RECORD") >>= drainCursor
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServer) docs
            mapM getStoreFm fileServers
	    fm <- searchFileMappings decryptedFN
	    file <- openFileQuery fileName fm
            return file

closeFile :: FileUpload -> ApiHandler Response
closeFile fileupload@(FileUpload ticket encryptedTimeout (File encryptedFN encryptedFC)) = liftIO $  do
  let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket
  let decryptedFN = encryptDecrypt sessionKey encryptedFN

  currentTime <- getCurrentTime
  if (currentTime > decryptedTimeout) then do
    putStrLn ("Client ticket timeout")
    let encryptedResponse = encryptDecrypt sessionKey "Session Timeout"
    return (Response encryptedResponse)
  else do
    fileMapping <- liftIO $ withMongoDbConnection $ do
      docs <- find (select ["fileName" =: decryptedFN] "FILEMAPPING_RECORD") >>= drainCursor
      return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
    case (length fileMapping) of
      0 -> do 
        putStrLn ("New File")
        fileServers <- liftIO $ withMongoDbConnection $ do
          docs <- find (select [] "FILESERVER_RECORD") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServer) docs
        let range = length fileServers
        index <- randomRIO (0, (range-1))
        let fs = fileServers !! index
        manager <- newManager defaultManagerSettings
        res <- runClientM (uploadQuery fileupload) (ClientEnv manager (BaseUrl Http (fsaddress fs) (read(fsport fs) :: Int) ""))
        case res of
          Left err -> do
            putStrLn ("File upload failed")
            let encryptedResponse = encryptDecrypt sessionKey "Failed"
            return (Response encryptedResponse)
          Right (Response response) -> do
            let decryptResponse = encryptDecrypt sessionKey response
            putStrLn "File uploaded"
            return (Response response)
      _ -> do
        let fm = head $ fileMapping
        manager <- newManager defaultManagerSettings
        res <- runClientM (uploadQuery fileupload) (ClientEnv manager (BaseUrl Http (fmaddress fm) (read(fmport fm) :: Int) ""))
        case res of
          Left err -> do
            putStrLn ("File upload failed")
            let encryptedResponse = encryptDecrypt sessionKey "Failed"
            return (Response encryptedResponse)
          Right (Response response) -> do
            let decryptResponse = encryptDecrypt sessionKey response
            putStrLn "File uploaded"
            return (Response response)
  
allFiles :: Ticket -> ApiHandler [String]
allFiles (Ticket ticket encryptedTimeout) = liftIO $ do
  putStrLn ("Checking User Credentials")
  let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket

  currentTime <- getCurrentTime
  if (currentTime > decryptedTimeout) then do
    putStrLn ("Client ticket timeout")
    let encryptedResponse = encryptDecryptArray sessionKey (["Failed", "Session key timeout"])
    return encryptedResponse
  else do
    putStrLn ("Client Credentials ok, fetching files")
    fileServers <- liftIO $ withMongoDbConnection $ do
      docs <- find (select [] "FILESERVER_RECORD") >>= drainCursor
      return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServer) docs
    mapM getStoreFm fileServers
    filemappings <- liftIO $ withMongoDbConnection $ do
      docs <- find (select [] "FILEMAPPING_RECORD") >>= drainCursor
      return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
    let filenames = map fmfileName filemappings
    let filenames' = DL.nub $ DL.sort filenames
    let encryptedResponse = encryptDecryptArray sessionKey filenames'
    return encryptedResponse

removeFileName :: FileName -> ApiHandler Response
removeFileName fileName@(FileName ticket encryptedTimeout encryptedFN) = liftIO $ do
          let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
          let sessionKey = encryptDecrypt sharedSecret ticket
          let decryptedFN = encryptDecrypt sessionKey encryptedFN

          putStrLn ("Checking client timeout")

          currentTime <- getCurrentTime
          if (currentTime > decryptedTimeout) then do
            putStrLn ("Clients token timed out")
            return (Response (encryptDecrypt sessionKey "Failed"))
          else do
            fileServers <- liftIO $ withMongoDbConnection $ do
              docs <- find (select [] "FILESERVER_RECORD") >>= drainCursor
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServer) docs
            mapM getStoreFm fileServers
            filemappings <- withMongoDbConnection $ do 
              docs <- find (select ["_id" =: decryptedFN] "FILEMAPPING_RECORD") >>= drainCursor
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileMapping) docs
            let filemap =  head $ filemappings
            manager <- newManager defaultManagerSettings
            let fileName = (FileName ticket encryptedTimeout encryptedFN)
            res <- runClientM (DirectoryServer.delete fileName) (ClientEnv manager (BaseUrl Http (fmaddress filemap) (read(fmport filemap) :: Int) ""))
            case res of
              Left err -> do
                return (Response (encryptDecrypt sessionKey (show err)))
                          
              Right response -> do 
                liftIO $ withMongoDbConnection $ do
                  Database.MongoDB.delete (select ["fileName" =: decryptedFN] "FILEMAPPING_RECORD")
                return (response)
  
        