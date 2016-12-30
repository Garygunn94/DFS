{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ClientProxyApi where
import              System.Random
import              Control.Monad.Trans.Except
import              Control.Monad.Trans.Resource hiding (register)
import              Control.Monad.IO.Class
import              Data.Aeson
import              Data.Aeson.TH
import              Data.Bson.Generic
import              GHC.Generics
import              Network.Wai hiding(Response)
import              Network.Wai.Handler.Warp
import              Network.Wai.Logger
import              Servant
import              Servant.API
import              Servant.Client
import              System.IO
import              System.Directory
import              System.Environment           (getArgs, getProgName, lookupEnv)
import              System.Log.Formatter
import              System.Log.Handler           (setFormatter)
import              System.Log.Handler.Simple
import              System.Log.Handler.Syslog
import              System.Log.Logger
import         	    Data.Bson.Generic
import qualified	Data.List                    as DL
import           	Data.Maybe                   (catMaybes)
import           	Data.Text                    (pack, unpack)
import           	Data.Time.Clock              (UTCTime, getCurrentTime)
import           	Data.Time.Format             (defaultTimeLocale, formatTime)
import              Control.Monad (when)
import              Network.HTTP.Client (newManager, defaultManagerSettings)
import              System.Process

data File = File { 
    fileName :: FilePath, 
    fileContent :: String 
} deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

data Response = Response{
  response :: String
} deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

data User = User{
    uusername :: String,
    upassword :: String,
  timeout :: String,
  token :: String
} deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User
instance ToBSON User
instance FromBSON User

data Signin = Signin{
  susername :: String,
  spassword :: String
} deriving (Eq, Show, Generic)
instance ToJSON Signin
instance FromJSON Signin
instance ToBSON Signin
instance FromBSON Signin

type ApiHandler = ExceptT ServantErr IO

serverport :: String
serverport = "8080"

serverhost :: String
serverhost = "localhost"

type AuthApi = 
    "signin" :> ReqBody '[JSON] Signin :> Post '[JSON] User :<|>
    "register" :> ReqBody '[JSON] Signin :> Post '[JSON] Response  :<|>
    "isvalid" :> ReqBody '[JSON] User :> Post '[JSON] Response

authApi :: Proxy AuthApi
authApi = Proxy

signin :: Signin -> ClientM User
register :: Signin -> ClientM Response
isvalid :: User -> ClientM Response

signin :<|> register :<|> isvalid = client authApi

signinQuery :: Signin -> ClientM User
signinQuery signindetails = do
  signinquery <- signin signindetails
  return signinquery

registerQuery :: Signin -> ClientM Response
registerQuery registerdetails = do
  registerquery <- register registerdetails
  return registerquery

isvalidQuery :: User -> ClientM Response
isvalidQuery isvaliddetails = do
  isvalidquery <- isvalid isvaliddetails
  return isvalidquery


type DirectoryApi = 
    "open" :> Capture "fileName" String :> Get '[JSON] File :<|>
    "close" :> ReqBody '[JSON] File :> Post '[JSON] Response :<|>
    "allfiles" :> Get '[JSON] [String]

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy


open :: String -> ClientM File
close :: File -> ClientM Response
allfiles :: ClientM [String]

open :<|> close :<|> allfiles = client directoryApi

openQuery:: String -> ClientM File
openQuery filename = do
	openquery <- open filename
	return openquery

closeQuery:: File -> ClientM Response
closeQuery file = do
	closequery <- close file
	return closequery

mainClient :: IO()
mainClient = do
  createDirectoryIfMissing True ("localstorage/")
  setCurrentDirectory ("localstorage/")
  authpart

authpart :: IO()
authpart = do
  putStrLn $ "Enter one of the following commands: LOGIN/REGISTER"
  cmd <- getLine
  case cmd of
    "LOGIN" -> authlogin
    "REGISTER" -> authregister

authlogin :: IO ()
authlogin = do
  putStrLn $ "Enter your username:"
  username <- getLine
  putStrLn $ "Enter your password"
  password <- getLine
  let user = (Signin username password)
  manager <- newManager defaultManagerSettings
  res <- runClientM (signinQuery user) (ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
  case res of
   Left err -> do putStrLn $ "Error: " ++ show err
                  authpart
   Right response -> mainloop response

authregister :: IO ()
authregister = do
  putStrLn $ "Enter your details to make a new account"
  putStrLn $ "Enter your username:"
  username <- getLine
  putStrLn $ "Enter your password"
  password <- getLine
  let user = (Signin username password)
  manager <- newManager defaultManagerSettings
  res <- runClientM (registerQuery user) (ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
  case res of
   Left err -> do putStrLn $ "Error: " ++ show err
                  authpart
   Right response -> authpart

mainloop :: User -> IO()
mainloop user = do
    putStrLn $ "Enter one of the following commands: FILES/UPLOAD/DOWNLOAD/CLOSE"
    cmd <- getLine
    case cmd of
        "FILES" -> displayFiles user
        "UPLOAD" -> uploadFile user
        "DOWNLOAD" -> downloadFile user
        "CLOSE" -> putStrLn $ "Closing service!"
        _ -> do putStrLn $ "Invalid Command. Try Again"
                mainloop user

displayFiles :: User -> IO()
displayFiles user = do
  putStrLn "Fetching file list. Please wait."
  isTokenValid user
  manager <- newManager defaultManagerSettings
  res <- runClientM allfiles (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
   Right response -> do mapM putStrLn response
                        mainloop user

uploadFile :: User -> IO()
uploadFile user = do
    putStrLn "Please enter the name of the file to upload"
    fileName <- getLine
    putStrLn "Please enter the contents of the file to upload"
    fileContent <- getLine
    let file = File fileName fileContent
    response <- putFile file user
    putStrLn $  "Response: " ++ show response
    mainloop user


downloadFile :: User -> IO()
downloadFile user = do
	putStrLn "Please enter the name of the file to download"
	fileName <- getLine
        getFile fileName user
	mainloop user

isTokenValid :: User -> IO()
isTokenValid user = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (isvalidQuery user) (ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
   Right responser -> do case (response responser) of
                            "Token is Valid" -> return()
                            _ -> do putStrLn $ "Session timeout, returning to login menu"
                                    authpart

getFile:: String -> User -> IO()
getFile filename user = do
  isTokenValid user
  manager <- newManager defaultManagerSettings
  res <- runClientM (openQuery filename) (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
                          
   Right response -> do liftIO (writeFile (fileName response) (fileContent response))
                     	let cmd = shell ("vim " ++ (fileName response))
	                createProcess_ "vim" cmd
                        putStrLn $ "Would you like to re-upload this file? y/n"
                        yesorno <- getLine
                        putStrLn $ "Are you Sure? y/n"
                        sure <- getLine
                     
                        case sure of
                         ("y") -> do fileContent <- readFile (fileName response)
                                     let file = File filename fileContent
                                     putFile file user
                                     mainloop user
                         (_) -> mainloop user

                                    
putFile:: File -> User-> IO ()
putFile file user = do
  isTokenValid user
  manager <- newManager defaultManagerSettings
  res <- runClientM (closeQuery file) (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
                          
   Right response -> putStrLn $ show response

