{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module FileServerRest where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           System.Directory
import           CommonServer
import           CommonServerAPI
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.API
import           Servant.Client

serverID = CServer "localhost" "8081"

resources :: String
resources = "res/FileServers";

fileApi :: Proxy FileApi
fileApi = Proxy

fileServer :: Server FileApi
fileServer =
    getFiles :<|>
    downloadFile :<|>
    uploadFile

fileApp :: Application
fileApp = serve fileApi fileServer

mkApp :: IO()
mkApp = do
  
  run 8081 fileApp 

getFiles :: APIHandler [FilePath]
getFiles = liftIO (getDirectoryContents resources)

downloadFile :: FilePath -> APIHandler CommonServer.File
downloadFile f = do    
    content <- liftIO (readFile (resources ++ "/" ++ f))
    return (File f content)

uploadFile :: File -> APIHandler CommonServer.Response
uploadFile (File f c) = do    
    liftIO (writeFile (resources ++ "/" ++ f) c)
    return (CommonServer.Response serverID)

fsHandshake :: IO()
fsHandshake = do
  manager <- newManager defaultManagerSettings
  res <- runClientM client (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (cserver) -> do
      print cserver

