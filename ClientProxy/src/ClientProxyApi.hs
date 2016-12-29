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
    "close" :> ReqBody '[JSON] File :> Post '[JSON] Response

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy


open :: String -> ClientM File
close :: File -> ClientM Response

open :<|> close = client directoryApi

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
   Right response -> do let tokener = token response
                        let timeouter = timeout response
                        mainloop tokener timeouter
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


mainloop :: String -> String -> IO()
mainloop tokener timeouter = do
    putStrLn $ "Enter one of the following commands: UPLOAD/DOWNLOAD/CLOSE"
    cmd <- getLine
    case cmd of
        "UPLOAD" -> uploadFile tokener timeouter
        "DOWNLOAD" -> downloadFile tokener timeouter
        "CLOSE" -> putStrLn $ "Closing service!"
        _ -> do putStrLn $ "Invalid Command. Try Again"
                mainloop tokener timeouter

uploadFile :: String -> String -> IO()
uploadFile tokener timeouter = do
    putStrLn "Please enter the name of the file to upload"
    fileName <- getLine
    putStrLn "Please enter the contents of the file to upload"
    fileContent <- getLine
    let file = File fileName fileContent
    response <- putFile file
    putStrLn $  "Response: " ++ show response
    mainloop tokener timeouter


downloadFile :: String -> String -> IO()
downloadFile tokener timeouter = do
	putStrLn "Please enter the name of the file to download"
	fileName <- getLine
        getFile fileName tokener timeouter
	mainloop tokener timeouter

getFile:: String -> String -> String -> IO()
getFile filename tokener timeouter = do
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
                                     putStrLn $ fileContent
                                     let file = File filename fileContent
                                     putFile file
                                     mainloop tokener timeouter
                         (_) -> mainloop tokener timeouter

                                    
putFile:: File -> IO ()
putFile file = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (closeQuery file) (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
                          
   Right response -> putStrLn $ show response

