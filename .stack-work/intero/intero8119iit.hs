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
import           System.IO
import           System.Directory
import           CommonServer
import           CommonServerAPI

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
mkApp = run 8081 fileApp 

getFiles :: APIHandler [FilePath]
getFiles = liftIO (getDirectoryContents resources)

downloadFile :: FilePath -> APIHandler CommonServer.File
downloadFile f = do    
    content <- liftIO (readFile (resources ++ "/" ++ f))
    return (File f content)

uploadFile :: File -> APIHandler CommonServer.Response
uploadFile (File f c) = do    
    liftIO (writeFile (resources ++ "/" ++ f) c)
    return (CommonServer.Response 0 serverID)




