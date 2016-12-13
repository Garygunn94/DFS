{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module IdentityServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Info
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           CommonServer
import           CommonServerApi
import           CommonServerApiClient

identity :: Identity
identity = Identity "localhost" "8081" CommonServer.IdentityServer

identityApi :: Proxy IdentityApi
identityApi = Proxy

identityServer :: Server IdentityApi
identityServer = 
    submit :<|>
    getNext :<|>
    getAll :<|>
    IdentityServer.getPort :<|>
    report

identityApp :: Application
identityApp = serve IdentityServer.identityApi identityServer

mkIdentityServer :: IO()
mkIdentityServer = run (read (port identity)::Int) identityApp

identities :: [Identity]
identities = []

getPorts :: [Int]
getPorts = map (port Identity) identities

submit :: Identity -> ApiHandler CommonServer.Response
submit i = do
    identities ++ [i]
    return (Response IdentityReceived identity)

getNext :: ServerType -> ApiHandler Identity
getNext st = liftIO (head (filter (\n -> (serverType n) == st) identities))

getAll :: ServerType -> ApiHandler [Identity]
getAll st = (liftIO (filter (\n -> (serverType n) == st) identities))

getPort :: ServerType -> ApiHandler Int
getPort st = return (maximum getPorts) + 1

report :: Identity -> ApiHandler CommonServer.Response
report i = do
    result <- i `elem` identities
    if result then return (Response IdentityFound i)
    else return (Response IdentityNotFound i)
