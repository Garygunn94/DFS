{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass       #-}

module FileServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai hiding(Response)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           System.Directory
import Network.HTTP.Client (newManager, defaultManagerSettings)
--import           CommonServer
--import           CommonServerApi
--import           CommonServerApiClient
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


type ApiHandler = ExceptT ServantErr IO

type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "upload" :> ReqBody '[JSON] File :> Post '[JSON] Response
    -- :<|>
type DirectoryApi = 
    "join" :> ReqBody '[JSON] FileServer :> Post '[JSON] Response
    
serverport :: String
serverport = "7007"

serverhost :: String
serverhost = "localhost"

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

join :: FileServer -> ClientM Response

join = client directoryApi

joinQuery :: FileServer -> ClientM Response
joinQuery fs = do
  get_join <- join fs
  return get_join
  
fileApi :: Proxy FileApi
fileApi = Proxy

files:: ClientM [FilePath]
download :: String -> ClientM File
upload :: File -> ClientM Response

files :<|> download :<|> upload = client fileApi
server :: Server FileApi
server = 
    getFiles :<|>
    downloadFile :<|>
    uploadFile

fileApp :: Application
fileApp = serve fileApi server

mkApp :: IO()
mkApp = do
   -- fsToDsHandshake
    createDirectoryIfMissing True ("fileserver" ++ serverhost ++ ":" ++ serverport ++ "/")
    setCurrentDirectory ("fileserver" ++ serverhost ++ ":" ++ serverport ++ "/")
    putStrLn $ "Attempting to join directory server"
    manager <- newManager defaultManagerSettings
    let fs = (FileServer (serverhost++serverport) serverhost serverport)
    res <- runClientM (joinQuery fs) (ClientEnv manager (BaseUrl Http serverhost (read(serverport) :: Int) ""))
    case res of
       Left err -> putStrLn $ "Error: " ++ show err
       Right response -> run (read (serverport) ::Int) fileApp 

getFiles :: ApiHandler [FilePath]
getFiles =
  liftIO(getDirectoryContents ("../fileserver" ++ serverhost ++ ":" ++ serverport ++ "/"))

downloadFile :: FilePath -> ApiHandler File
downloadFile f = do    
    content <- liftIO (readFile f)
    return (File f content)

uploadFile :: File -> ApiHandler Response
uploadFile (File f c) = do    
    liftIO (writeFile f c)
    return (Response "Success")

