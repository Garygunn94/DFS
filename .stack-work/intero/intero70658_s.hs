{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServerAPI where
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Network.Wai
import           Servant
import           CommonServer


type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "upload" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response

type APIHandler = ExceptT ServantErr IO

type DirectoryApi = 
    "files" :> Get '[JSON] [String] :<|>
    "files" :> Capture "uuid" String :> Get '[JSON] [String] :<|>
    "open" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "close" :> ReqBody '[JSON] File :> Post '[JSON] File
