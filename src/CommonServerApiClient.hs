{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module CommonServerApiClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           CommonServerApi
import           CommonServer
import Network.HTTP.Client hiding(Proxy, Response)

identityApi :: Proxy IdentityApi
identityApi = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

idsubmit :: Identity -> Manager -> BaseUrl -> ClientM Response
idnext :: ServerType -> Manager -> BaseUrl -> ClientM Identity
idall :: ServerType -> Manager -> BaseUrl -> ClientM [Identity]
idport :: ServerType -> Manager -> BaseUrl -> ClientM Int
idreport :: Identity -> Manager -> BaseUrl -> ClientM Response

-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

idsubmit :<|> idnext :<|> idall :<|> idport :<|> idreport =  Servant.Client.client identityApi


