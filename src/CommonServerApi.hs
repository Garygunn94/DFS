{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServerApi where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           Network.Wai
import           Servant
import           CommonServer

type ApiHandler = ExceptT ServantErr IO

type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "upload" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response -- :<|>
--    "beginTrans" :> Get '[JSON] CommonServer.Response :<|>
--    "endTrans" :> Get '[JSON] CommonServer.Response :<|>
--    "commitTrans" :> Get '[JSON] CommonServer.Response

type DirectoryApi = 
    "files" :> Get '[JSON] [String] :<|>
    "files" :> Capture "uuid" String :> Get '[JSON] [String] :<|>
    "open" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "close" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response :<|>
    "beginTrans" :> Get '[JSON] CommonServer.Response :<|>
    "endTrans" :> Get '[JSON] CommonServer.Response :<|>
    "commitTrans" :> Get '[JSON] CommonServer.Response

type SecurityApi =
    "login" :> ReqBody '[JSON] CommonServer.Client :> Post '[JSON] CommonServer.Token :<|>
    "session" :> Capture "id" String :> Get '[JSON] CommonServer.Token
    
type ProxyApi =
    "files" :> Get '[JSON] [String] :<|>
    "open" :> Capture "fileName" String :> Get '[JSON] CommonServer.File :<|>
    "close" :> ReqBody '[JSON] CommonServer.File :> Post '[JSON] CommonServer.Response :<|>
    "close" :> Capture "fileName" String :> Get '[JSON] CommonServer.Response

type TransactionApi =
    "beginT" :> Get '[JSON] CommonServer.Response :<|>
    "endT" :> Get '[JSON] CommonServer.Response :<|>
    "statusT" :> Get '[JSON] CommonServer.Response

type IdentityApi =
         "submit" :> ReqBody '[JSON] CommonServer.Identity :> Post '[JSON] CommonServer.Response
    :<|> "next" :> ReqBody '[JSON] CommonServer.ServerType :> Post '[JSON] CommonServer.Identity
    :<|> "all" :> ReqBody '[JSON] CommonServer.ServerType :> Post '[JSON] [CommonServer.Identity]
    :<|> "port" :> ReqBody '[JSON] CommonServer.ServerType :> Post '[JSON] Int 
    :<|> "report" :> ReqBody '[JSON] CommonServer.Identity :> Post '[JSON] CommonServer.Response

