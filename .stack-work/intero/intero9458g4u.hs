{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module FileServerRestApiClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           CommonServerAPI
import           CommonServer


shakeAPI :: Proxy HandshakeApi
shakeAPI = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

shake :: CServer -> ClientM Int


-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

shake =  client shakeAPI
