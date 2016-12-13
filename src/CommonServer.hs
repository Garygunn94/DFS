{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServer where
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
------------------------------
--  File Structure
------------------------------
data File = File { 
    fileName :: FilePath, 
    fileContent :: String 
} deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

------------------------------
--  Server Identity 
------------------------------
data Identity = Identity { 
    address :: String, 
    port :: String,
    serverType :: ServerType
} deriving (Eq, Show, Generic)

instance ToJSON Identity
instance FromJSON Identity

------------------------------
--  Registered Server Types 
------------------------------

data ServerType = 
    FileServer |
    DirectoryServer |
    ProxyServer |
    SecurityServer |
    TransactionServer |
    IdentityServer |
    ReplicationServer
    deriving(Eq, Show, Generic)
instance ToJSON ServerType
instance FromJSON ServerType

------------------------------
--  Resources Directory 
------------------------------
data Resources = Resources { 
    path :: String     
} deriving (Eq, Show, Generic)

instance ToJSON Resources
instance FromJSON Resources

------------------------------
--  Client Data
------------------------------
data Client = Client {
    username :: String,
    password :: String
} deriving (Eq, Show, Generic)
instance ToJSON Client
instance FromJSON Client
------------------------------
--  Security Token
------------------------------
data Token = Token {
    sessionId :: String,
    sessionKey :: String,
    ticket :: String,
    client :: Identity
} deriving (Eq, Show, Generic)
instance ToJSON Token
instance FromJSON Token

------------------------------
--  Response Packet 
------------------------------
data Response = Response { 
    code :: ResponseCode, 
    server :: Identity 
} deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

------------------------------
--  Response Codes 
------------------------------
data ResponseCode = 
    FileUploadComplete |
    FileUploadError |
    HandshakeSuccessful |
    HandshakeError |
    IdentityFound |
    IdentityNotFound |
    IdentityReceived
    deriving(Eq, Show, Generic)
instance ToJSON ResponseCode
instance FromJSON ResponseCode
