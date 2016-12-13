{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module FileServer where

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
import           CommonServer
import           CommonServerApi
import           CommonServerApiClient

identity = Identity "localhost" "8081" FileServer

resources :: Resources
resources = Resources "res/FileServers";

fileApi :: Proxy FileApi
fileApi = Proxy

server :: Server FileApi
server = 
    getFiles :<|>
    downloadFile :<|>
    uploadFile

fileApp :: Application
fileApp = serve fileApi FileServer.server

mkApp :: IO()
mkApp = do
   -- fsToDsHandshake
    run (read (port identity)::Int) fileApp 

getFiles :: ApiHandler [FilePath]
getFiles = liftIO (getDirectoryContents (path resources))

downloadFile :: FilePath -> ApiHandler File
downloadFile f = do    
    content <- liftIO (readFile ((path resources) ++ "/" ++ f))
    return (File f content)

uploadFile :: File -> ApiHandler CommonServer.Response
uploadFile (File f c) = do    
    liftIO (writeFile ((path resources) ++ "/" ++ f) c)
    return (CommonServer.Response FileUploadComplete identity)

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


