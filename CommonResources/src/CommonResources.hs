{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommonResources where

import            Data.Aeson
import            Data.Aeson.TH
import            Data.Bits
import            Data.Bson.Generic
import            Data.Char
import            Data.Time
import            GHC.Generics
import            Servant

authserverport :: String
authserverport = "8083"

authserverhost :: String
authserverhost = "localhost"

dirserverport :: String
dirserverport = "8080"

dirserverhost :: String
dirserverhost = "localhost"

fsserverport :: String
fsserverport = "7007"

fsserverhost :: String
fsserverhost = "localhost"

lockserverport :: String
lockserverport = "8084"

lockserverhost :: String
lockserverhost = "localhost"

transserverhost :: String
transserverhost = "localhost"

transserverport :: String
transserverport = "8085"

type DirectoryApi = 
    "join" :> ReqBody '[JSON] FileServer :> Post '[JSON] Response :<|>
    "open" :> ReqBody '[JSON] FileName :> Post '[JSON] File :<|>
    "close" :> ReqBody '[JSON] FileUpload :> Post '[JSON] Response :<|>
    "allfiles" :> ReqBody '[JSON] Ticket :> Post '[JSON] [String] :<|>
    "remove" :> ReqBody '[JSON] FileName :> Post '[JSON] Response

type AuthApi = 
    "signin" :> ReqBody '[JSON] Signin :> Post '[JSON] Session :<|>
    "register" :> ReqBody '[JSON] Signin :> Post '[JSON] Response
   {-} "isvalid" :> ReqBody '[JSON] User :> Post '[JSON] Response :<|>
    "extend" :> ReqBody '[JSON] User :> Post '[JSON] Response-}

type FileApi = 
    "files" :> Get '[JSON] [FilePath] :<|>
    "download" :> ReqBody '[JSON] FileName :> Post '[JSON] File :<|>
    "upload" :> ReqBody '[JSON] FileUpload :> Post '[JSON] Response :<|>
    "delete" :> ReqBody '[JSON] FileName :> Post '[JSON] Response

type LockingApi = 
    "lock" :> ReqBody '[JSON] FileName :> Post '[JSON] Response :<|>
    "unlock" :> ReqBody '[JSON] FileName :> Post '[JSON] Response :<|>
    "islocked" :> ReqBody '[JSON] FileName :> Post '[JSON] Response

type TransactionApi = 
    "begin" :> ReqBody '[JSON] Ticket :> Post '[JSON] Response :<|>
    "transDownload" :> ReqBody '[JSON] FileName :> Post '[JSON] File :<|>
    "transUpload" :> ReqBody '[JSON] FileUpload :> Post '[JSON] Response :<|>
    "transcommit" :> ReqBody '[JSON] Ticket :> Post '[JSON] Response

data File = File { 
    fileName :: FilePath, 
    fileContent :: String 
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FileUpload = FileUpload {
 futicket :: String,
 encTimeout :: String,
 encryptedFile :: File
} deriving (Show, Generic, FromJSON, ToJSON)

data FileName = FileName {
    fnticket :: String,
    fnencryptedTimeout :: String,
    encryptedFN :: String
} deriving (Show, Generic, FromJSON, ToJSON)

data Response = Response{
  response :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FileServer = FileServer{
    id :: String,
    fsaddress :: String,
    fsport :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FileMapping = FileMapping{
	fmfileName :: String,
	fmaddress :: String,
	fmport :: String
} deriving (Eq, Show, Generic,ToJSON, FromJSON, ToBSON, FromBSON)


data Lock = Lock{
	lockfileName :: String,
	locked :: Bool
} deriving(Eq, Show, Generic, ToBSON, FromBSON)

data Account = Account{
    username :: String,
    password :: String
} deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

data Session = Session{
    encryptedTicket :: String,
    encryptedSessionKey :: String,
	encryptedTokenTimeout :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data Signin = Signin{
	susername :: String,
	sencryptedMsg :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data Ticket = Ticket{
    ticket :: String,
    encryptedTimeout :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data TransactionFile = TransactionFile{
    transFileName :: String,
    userID :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)
deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

deriving instance FromBSON Bool  -- we need these as BSON does not provide
deriving instance ToBSON Bool

encryptDecrypt :: String -> String -> String
encryptDecrypt key text = zipWith (\a b -> chr $ xor (ord a) (ord b)) (cycle key) text
-- XOR each element of the text with a corresponding element of the key

encryptTime :: String  -> UTCTime  -> String
encryptTime key time = encryptDecrypt key (show(time) :: String)

decryptTime :: String  -> String  -> UTCTime
decryptTime key text = (read $ encryptDecrypt key text) :: UTCTime

encryptPort :: String  -> Int  -> String
encryptPort key port = encryptDecrypt key (show(port) :: String)

decryptPort :: String  -> String  -> Int
decryptPort key text = (read $ encryptDecrypt key text) :: Int

encryptDecryptArray :: String -> [String] -> [String]
encryptDecryptArray key array = do
  encryptedArray <- map (encryptDecrypt key) array
  return encryptedArray

logMessage :: Bool -> String -> IO()
logMessage logBool message = do
  if(logBool) then putStrLn message
  else return ()

sharedSecret :: String
sharedSecret = "Maybe I'll complete this project one day"