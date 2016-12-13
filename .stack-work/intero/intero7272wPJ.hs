{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryServerRest where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import Data.Proxy
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import           Servant
import           System.IO
import           System.Directory
import qualified Data.Map as M
import Control.Concurrent.STM
import           CommonServer
import           CommonServerAPI

type Uuid = Int
type Address = String
type Port = String
type Filename = String
type Timestamp = IO String

--Server data type allows me to pass address and port details easily
data DirectoryServer = DirectoryServer
  { server :: CServer 
  , filemappings   :: TVar (M.Map Filename Filemapping)
  , fileservers :: TVar (M.Map Uuid Fileserver)
  , fileservercount :: TVar Int
}

data Filemapping = Filemapping
  { fmfilename :: Filename
  , fmuuid     :: Uuid
  , fmtimestamp :: Timestamp
  }

data Fileserver = Fileserver
  { fsuuid :: Uuid,
    fserver :: CServer
  }

files :: ClientM String
download :: Maybe String -> ClientM File
upload :: File -> ClientM CommonServer.Response


directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

directoryServerApi :: Server DirectoryApi
directoryServerApi = 
    getFiles :<|>
    getFilesFrom :<|>
    openFile :<|>
    closeFile :<|>
    handshake

directoryClientApi :: Proxy FileApi
directoryClientApi = Proxy

files :<|> download :<|> upload = client directoryClientApi

directoryApp :: Application
directoryApp = serve directoryApi directoryServerApi

handshakeApi :: Proxy HandshakeApi
handshakeApi = Proxy

handshakeserverApi :: Server HandshakeApi
handshakeserverApi =
  shake

handshakeApp :: Application
handshakeApp = serve handshakeApi handshakeserverApi 

mkApp :: IO()
mkApp = do
  directoryServer <- newDirectoryServer "localhost" "8082"
  run 8082 directoryApp
  run 8083 hanshakeApp

getFiles :: APIHandler [FilePath]
getFiles = do
  let x = (DirectoryServer a b c)
  elements <- elems directoryServer
  
  

getFilesFrom :: Uuid -> APIHandler [FilePath]
getFilesFrom x = do
  fs <- lookupFileServer directoryServer x
  case fs of
    (Just fs) -> do
      manager <- newManager defaultManagerSettings
      result <- runClientM (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
      case result of
        Right (files) -> do
          return files

openFile :: 

newDirectoryServer :: String -> String -> IO DirectoryServer
newDirectoryServer address port = atomically $ do DirectoryServer <$> return address <*> return port <*> newTVar M.empty <*> newTVar M.empty <*> newTVar 0 

--lookupFileserver :: DirectoryServer -> Uuid -> STM (Maybe Fileserver)
--lookupFileserver DirectoryServer{..} uuid = M.lookup uuid <$> readTVar fileservers
