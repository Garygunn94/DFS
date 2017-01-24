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
import CommonResources


type ApiHandler = ExceptT ServantErr IO


directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

join :: FileServer -> ClientM Response
open :: String -> ClientM File
close :: File -> ClientM Response
allfiles :: ClientM [String]

join :<|> open :<|> close :<|> allfiles = client directoryApi

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
    createDirectoryIfMissing True ("fileserver" ++ fsserverhost ++ ":" ++ fsserverport ++ "/")
    setCurrentDirectory ("fileserver" ++ fsserverhost ++ ":" ++ fsserverport ++ "/")
    putStrLn $ "Attempting to join directory server"
    manager <- newManager defaultManagerSettings
    let fs = (FileServer (fsserverhost++fsserverport) fsserverhost fsserverport)
    res <- runClientM (joinQuery fs) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
    case res of
       Left err -> putStrLn $ "Error: " ++ show err
       Right response -> run (read (fsserverport) ::Int) fileApp 

getFiles :: ApiHandler [FilePath]
getFiles =
  liftIO(getDirectoryContents ("../fileserver" ++ fsserverhost ++ ":" ++ fsserverport ++ "/"))

downloadFile :: FilePath -> ApiHandler File
downloadFile f = do    
    content <- liftIO (readFile f)
    return (File f content)

uploadFile :: File -> ApiHandler Response
uploadFile (File f c) = do    
    liftIO (writeFile f c)
    return (Response "Success")

