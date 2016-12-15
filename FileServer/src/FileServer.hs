{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

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

type ApiHandler = ExceptT ServantErr IO

type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "upload" :> ReqBody '[JSON] File :> Post '[JSON] Response -- :<|>

serverport :: String
serverport = "7007"

serverhost :: String
serverhost = "localhost"

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
    run (read (serverport) ::Int) fileApp 

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

