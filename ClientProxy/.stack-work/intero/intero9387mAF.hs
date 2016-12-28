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

module ClientProxyApi where
import              System.Random
import              Control.Monad.Trans.Except
import              Control.Monad.Trans.Resource
import              Control.Monad.IO.Class
import              Data.Aeson
import              Data.Aeson.TH
import              Data.Bson.Generic
import              GHC.Generics
import              Network.Wai hiding(Response)
import              Network.Wai.Handler.Warp
import              Network.Wai.Logger
import              Servant
import              Servant.API
import              Servant.Client
import              System.IO
import              System.Directory
import              System.Environment           (getArgs, getProgName, lookupEnv)
import              System.Log.Formatter
import              System.Log.Handler           (setFormatter)
import              System.Log.Handler.Simple
import              System.Log.Handler.Syslog
import              System.Log.Logger
import         	    Data.Bson.Generic
import qualified	Data.List                    as DL
import           	Data.Maybe                   (catMaybes)
import           	Data.Text                    (pack, unpack)
import           	Data.Time.Clock              (UTCTime, getCurrentTime)
import           	Data.Time.Format             (defaultTimeLocale, formatTime)
import              Control.Monad (when)
import              Network.HTTP.Client (newManager, defaultManagerSettings)
import              System.Process

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

type ApiHandler = ExceptT ServantErr IO

serverport :: String
serverport = "8080"

serverhost :: String
serverhost = "localhost"

type DirectoryApi = 
    "open" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "close" :> ReqBody '[JSON] File :> Post '[JSON] Response

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy


open :: String -> ClientM File
close :: File -> ClientM Response

open :<|> close = client directoryApi

openQuery:: String -> ClientM File
openQuery filename = do
	openquery <- open fileName
	return openQuery

closeQuery:: File -> Response
closeQuery file = do
	closequery <- close file
	return closequery

--type ClientApi = 
--	"get" :> Capture "fileName" String :> Get '[JSON] File :<|>
--	"put" :> ReqBody '[JSON] File :> Post '[JSON] Response

--clientApi :: Proxy ClientApi
--clientApi = Proxy

--server :: Server ClientApi
--server = 
--    getFile :<|>
--    putFile

--clientApp :: Application
--clientApp = serve clientApi server

mkApp :: IO()
mkApp = do
    createDirectoryIfMissing True ("tmp/")
    setCurrentDirectory ("tmp/")
    putStrLn $ "Enter one of the following commands: UPLOAD/DOWNLOAD/CLOSE"
    cmd <- getLine
    case cmd of
        "UPLOAD" -> uploadFile
        "DOWNLOAD" -> downloadFile
        "CLOSE" -> putStrLn $ "Closing service!"
        _ -> do putStrLn $ "Invalid Command. Try Again"
                mkApp

uploadFile :: IO()
uploadFile = do
    putStrLn "Please enter the name of the file to upload"
    fileName <- getLine
    putStrLn "Please enter the contents of the file to upload"
    fileContent <- getLine
    let file = File fileName fileContent
    response <- putFile file
    putStrLn "Response: " ++ response
    mkApp 

downloadFile :: IO()
downloadFile = do
	putStrLn "Please enter the name of the file to download"
	fileName <- getLine
	response <- getFile fileName
	liftIO (writeFile (fileName response) (fileContent response))
	cmd <- shell ("vim " ++ fileName)
	result <- createProcess cmd
	mkApp

getFile:: String -> ApiHandler File
getFile filename = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (openQuery filename) (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
                          
   Right response -> return (response)

putFile:: File -> ApiHandler Response
putFile file = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (closeQuery file) (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
                          
   Right response -> return (response)

