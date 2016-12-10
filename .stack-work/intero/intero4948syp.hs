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

data FileServer = FileServer { address :: String, port :: Int, name :: String }

resources :: String
resources = "res/FileServers";

type FileApi = 
    "files" :> Get '[JSON] [String] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "upload" :> ReqBody '[JSON] File :> Post '[JSON] File

type FHandler = ExceptT ServantErr IO

data File = File { fileName :: FilePath, fileContent :: String } deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

fileApi :: Proxy FileApi
fileApi = Proxy

server :: Server FileApi
server = 
    getFiles :<|>
    downloadFile :<|>
    uploadFile

fileApp :: Application
fileApp = serve fileApi server

mkApp :: IO()
mkApp = run 8081 fileApp 

getFiles :: FHandler [FilePath]
getFiles = liftIO (getDirectoryContents resources)

downloadFile :: FilePath -> FHandler File
downloadFile f = do    
    content <- liftIO (readFile (resources ++ "/" ++ f))
    return (File f content)

uploadFile :: File -> FHandler File
uploadFile (File f c) = do    
    liftIO (writeFile (resources ++ "/" ++ f) c)
    return (File f c)




