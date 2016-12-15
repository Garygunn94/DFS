{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module FileServerRest where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
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
instance FromJson Response

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
    run (read (port identity)::Int) fileApp 

getFiles :: ApiHandler [FilePath]
getFiles = liftIO (getDirectoryContents (path resources))

downloadFile :: FilePath -> ApiHandler File
downloadFile f = do    
    content <- liftIO (readFile f)
    return (File f content)

uploadFile :: File -> ApiHandler Response
uploadFile (File f c) = do    
    liftIO (writeFile f c)
    return (Response "Success")

--fsToDsHandshake :: IO()
--fsToDsHandshake = do
   -- manager <- newManager defaultManagerSettings
   -- result <- runClientM identity (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
   -- case result of
    --    Left err -> putStrLn $ "Error: " ++ show err
     --   Right (Response code id) ->
      --      case code of
       --         HandshakeSuccessful -> return ()
        --        HandshakeError -> fsToDsHandshake


