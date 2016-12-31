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
import              LRUCache as C

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
    "isvalid" :> ReqBody '[JSON] User :> Post '[JSON] Response :<|>
    "extend" :> ReqBody '[JSON] User :> Post '[JSON] Response

authApi :: Proxy AuthApi
authApi = Proxy

signin :: Signin -> ClientM User
register :: Signin -> ClientM Response
isvalid :: User -> ClientM Response
extend :: User -> ClientM Response

signin :<|> register :<|> isvalid :<|> extend = client authApi

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

extendQuery :: User -> ClientM Response
extendQuery extenddetails = do
  extendquery <- extend extenddetails
  return extendquery


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

type LockingApi = 
    "lock" :> Capture "fileName" String :> Get '[JSON] Bool :<|>
    "unlock" :> Capture "fileName" String :> Get '[JSON] Bool :<|>
    "islocked" :> Capture "fileName" String :> Get '[JSON] Bool

lockingApi :: Proxy LockingApi
lockingApi = Proxy

lock :: String -> ClientM Bool
unlock :: String -> ClientM Bool
islocked :: String -> ClientM Bool

lock :<|> unlock :<|> islocked = client lockingApi

lockQuery:: String -> ClientM Bool
lockQuery fName = do
  lockquery <- lock fName
  return lockquery

unlockQuery:: String -> ClientM Bool
unlockQuery fName = do
  unlockquery <- unlock fName
  return unlockquery

islockedQuery :: String -> ClientM Bool
islockedQuery fName = do
  islockedquery <- islocked fName
  return islockedquery

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
   Right response -> do cache <- C.newHandle 5 
                        mainloop response cache

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

mainloop :: User -> (C.Handle String String) -> IO()
mainloop user cache = do
    putStrLn $ "Enter one of the following commands: FILES/UPLOAD/DOWNLOAD/CLOSE"
    cmd <- getLine
    case cmd of
        "FILES" -> displayFiles user cache
        "UPLOAD" -> uploadFile user cache
        "DOWNLOAD" -> downloadFile user cache
        "CLOSE" -> putStrLn $ "Closing service!"
        _ -> do putStrLn $ "Invalid Command. Try Again"
                mainloop user cache

displayFiles :: User -> (C.Handle String String) -> IO()
displayFiles user cache = do
  putStrLn "Fetching file list. Please wait."
  isTokenValid user
  manager <- newManager defaultManagerSettings
  res <- runClientM allfiles (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
   Right response -> do extendToken user
                        mapM putStrLn response
                        mainloop user cache

uploadFile :: User -> (C.Handle String String) -> IO()
uploadFile user cache = do
    putStrLn "Please enter the name of the file to upload"
    fileName <- getLine
    let cmd = shell ("vim " ++ fileName)
    createProcess_ "vim" cmd
    putStrLn $ "Hit enter when youre finished"
    enter <- getLine
    fileContent <- readFile fileName
    let file = File fileName fileContent
    response <- putFile file user cache
    putStrLn $  "Response: " ++ show response
    mainloop user cache


downloadFile :: User -> (C.Handle String String) -> IO()
downloadFile user cache = do
  putStrLn "Please enter the name of the file to download"
  fileName <- getLine
  incache <- C.iolookup cache fileName
  case incache of
    (Nothing) -> getFile fileName user cache
    (Just v) -> do putStrLn $ "Cache hit"
                   liftIO (writeFile (fileName) v)
                   let cmd = shell ("vim " ++ fileName)
                   createProcess_ "vim" cmd
                   putStrLn $ "Would you like to re-upload this file? y/n"
                   yesorno <- getLine
                   putStrLn $ "Are you Sure? y/n"
                   sure <- getLine
                   fileContent <- readFile (fileName)
                   case sure of
                     ("y") -> do let file = File fileName fileContent
                                 putFile file user cache
                                 mainloop user cache
                     (_) -> mainloop user cache
  mainloop user cache

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

extendToken :: User -> IO()
extendToken user = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (extendQuery user) (ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
   Right response -> return()


getFile:: String -> User -> (C.Handle String String) -> IO()
getFile filename user cache = do
  isTokenValid user
  locksuccess <- lockFile filename
  case locksuccess of
    True -> do
             manager <- newManager defaultManagerSettings
             res <- runClientM (openQuery filename) (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
             case res of
              Left err -> putStrLn $ "Error: " ++ show err
                          
              Right response -> do    extendToken user
                                      C.ioinsert cache filename (fileContent response) 
                                      liftIO (writeFile (fileName response) (fileContent response))
                     	              let cmd = shell ("vim " ++ (fileName response))
	                              createProcess_ "vim" cmd
                                      putStrLn $ "Would you like to re-upload this file? y/n"
                                      yesorno <- getLine
                                      putStrLn $ "Please enter your answer again y/n"
                                      sure <- getLine
                                      case sure of
                                          ("y") -> do unlocker <- unlockFile filename
                                                      fileContent <- readFile (fileName response)
                                                      let file = File filename fileContent
                                                      putFile file user cache
                                                      mainloop user cache
                                          (_) -> do unlocker <- unlockFile filename
                                                    mainloop user cache
                                           
    False -> putStrLn $ "Unable to lock file " ++ filename ++ ". Perhaps another user is using it."

                                    
putFile:: File -> User-> (C.Handle String String) -> IO ()
putFile file user cache = do
  isTokenValid user
  locksuccess <- lockFile (fileName file)
  case locksuccess of
    True -> do manager <- newManager defaultManagerSettings
               res <- runClientM (closeQuery file) (ClientEnv manager (BaseUrl Http "localhost" 7008 ""))
               case res of
                 Left err -> putStrLn $ "Error: " ++ show err
                          
                 Right responser -> do extendToken user
                                       unlocksuccess <- unlockFile (fileName file)
                                       case unlocksuccess of 
                                            True -> do incache <- C.iolookup cache (fileName file)
                                                       case incache of 
                                                          (Nothing) -> putStrLn $ (response responser)
                                                          (Just v) -> C.ioinsert cache (fileName file) (fileContent file)

                                            False -> putStrLn $ "Failed to unlock file possible conflict. Try again soon"

    False -> putStrLn $ "Unable to lock file " ++ (fileName file) ++ ". Perhaps another user is using it."


lockFile :: String -> IO Bool
lockFile fName = do 
  manager <- newManager defaultManagerSettings
  res <- runClientM (islockedQuery fName) (ClientEnv manager (BaseUrl Http "localhost" 8000 ""))
  case res of
   Left err -> do putStrLn $ "Error: " ++ show err
                  return False
                          
   Right responser -> do case responser of
                           True -> return False
                           False -> do res <- runClientM (lockQuery fName) (ClientEnv manager (BaseUrl Http "localhost" 8000 ""))
                                       case res of
                                          Left err ->do putStrLn $ "Error: " ++ show err
                                                        return False
                                          Right response -> return True


unlockFile :: String -> IO Bool
unlockFile fName = do 
  manager <- newManager defaultManagerSettings
  res <- runClientM (islockedQuery fName) (ClientEnv manager (BaseUrl Http "localhost" 8000 ""))
  case res of
   Left err -> do putStrLn $ "Error: " ++ show err
                  return False
                          
   Right responser -> do case responser of
                           False -> return False
                           True -> do res <- runClientM (unlockQuery fName) (ClientEnv manager (BaseUrl Http "localhost" 8000 ""))
                                      case res of
                                          Left err -> do putStrLn $ "Error: " ++ show err
                                                         return False
                                          Right response -> return True

                        
