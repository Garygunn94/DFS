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
import CommonResources


type ApiHandler = ExceptT ServantErr IO

serverport :: String
serverport = "8080"

serverhost :: String
serverhost = "localhost"


authApi :: Proxy AuthApi
authApi = Proxy

signin :: Signin -> ClientM Session
register :: Signin -> ClientM Response

signin :<|> register = client authApi

signinQuery :: Signin -> ClientM Session
signinQuery signindetails = do
  signinquery <- signin signindetails
  return signinquery

registerQuery :: Signin -> ClientM Response
registerQuery registerdetails = do
  registerquery <- register registerdetails
  return registerquery

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

join :: FileServer -> ClientM Response
open :: FileName -> ClientM File
close :: FileUpload -> ClientM Response
allfiles :: Ticket -> ClientM [String]

join :<|> open :<|> close :<|> allfiles = client directoryApi

openQuery:: FileName -> ClientM File
openQuery filename = do
	openquery <- open filename
	return openquery

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
  let encryptedUname = encryptDecrypt password username
  let user = (Signin username encryptedUname)
  manager <- newManager defaultManagerSettings
  res <- runClientM (signinQuery user) (ClientEnv manager (BaseUrl Http authserverhost (read(authserverport) :: Int) ""))
  case res of
   Left err -> do putStrLn $ "Error: " ++ show err
                  authpart
   Right response -> do 
    let (Session encryptedTicket encryptedSessionKey encryptedTimeout) = response
    case encryptedTicket of
      "Failed" -> do
        logMessage True ("Could not login user")
        authpart
      _ -> do
        logMessage True ("Client Login Successful")
        let decryptedTicket = encryptDecrypt password encryptedTicket
        let decryptedSessionKey = encryptDecrypt password encryptedSessionKey
        cache <- C.newHandle 5 
        mainloop (Session decryptedTicket decryptedSessionKey encryptedTimeout) cache

authregister :: IO ()
authregister = do
  putStrLn $ "Enter your details to make a new account"
  putStrLn $ "Enter your username:"
  username <- getLine
  putStrLn $ "Enter your password"
  password <- getLine
  let user = (Signin username password)
  manager <- newManager defaultManagerSettings
  res <- runClientM (registerQuery user) (ClientEnv manager (BaseUrl Http authserverhost (read(authserverport) :: Int) ""))
  case res of
   Left err -> do putStrLn $ "Error: " ++ show err
                  authpart
   Right response -> authpart

mainloop :: Session -> (C.Handle String String) -> IO()
mainloop session cache = do
    putStrLn $ "Enter one of the following commands: FILES/UPLOAD/DOWNLOAD/CLOSE"
    cmd <- getLine
    case cmd of
        "FILES" -> displayFiles session cache
        "UPLOAD" -> uploadFile session cache
        "DOWNLOAD" -> downloadFile session cache
        "CLOSE" -> putStrLn $ "Closing service!"
        _ -> do putStrLn $ "Invalid Command. Try Again"
                mainloop session cache

displayFiles :: Session -> (C.Handle String String) -> IO()
displayFiles session@(Session ticket sessionKey encryptedTimeout) cache = do
  putStrLn "Fetching file list. Please wait."
  manager <- newManager defaultManagerSettings
  res <- runClientM (allfiles (Ticket ticket encryptedTimeout)) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
   Right fileEncrypt -> do 
    let decryptedFiles = encryptDecryptArray sessionKey fileEncrypt
    mapM putStrLn decryptedFiles
    mainloop session cache

uploadFile :: Session -> (C.Handle String String) -> IO()
uploadFile session@(Session ticket sessionKey encryptedTimeout) cache = do
    putStrLn "Please enter the name of the file to upload"
    fileName <- getLine
    let cmd = shell ("vim " ++ fileName)
    createProcess_ "vim" cmd
    putStrLn $ "Hit enter when youre finished"
    enter <- getLine
    fileContent <- readFile fileName
    let encryptedFN = encryptDecrypt sessionKey fileName
    let encryptedFC = encryptDecrypt sessionKey fileContent
    manager <- newManager defaultManagerSettings
    res <- runClientM (close (FileUpload ticket encryptedTimeout (File encryptedFN encryptedFC))) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right (uploadResponse@(Response encryptedResponse)) -> do
        C.ioinsert cache (fileName) (fileContent)
        let decryptResponse = encryptDecrypt sessionKey encryptedResponse
        putStrLn decryptResponse
        mainloop session cache


downloadFile :: Session -> (C.Handle String String) -> IO()
downloadFile session@(Session ticket sessionKey encryptedTimeout) cache = do
  putStrLn "Please enter the name of the file to download"
  fileName <- getLine
  incache <- C.iolookup cache fileName
  case incache of
    (Nothing) -> do
      manager <- newManager defaultManagerSettings
      res <- runClientM (openQuery (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName))) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err                 
        Right response -> do 
          let decryptedFC = encryptDecrypt sessionKey (fileContent response)
          C.ioinsert cache fileName decryptedFC 
          liftIO (writeFile fileName decryptedFC)
          let cmd = shell ("vim " ++ fileName)
          createProcess_ "vim" cmd
          putStrLn $ "When finished please press Enter"
          yesorno <- getLine
          putStrLn $ "Would you like to upload your changes? y/n"
          sure <- getLine
          case sure of
            ("y") -> do 
              fileContent <- readFile fileName
              let file = File (encryptDecrypt sessionKey fileName) (encryptDecrypt sessionKey fileContent)
              manager <- newManager defaultManagerSettings
              res <- runClientM (close (FileUpload ticket encryptedTimeout file)) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
              case res of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (uploadResponse@(Response encryptedResponse)) -> do
                  C.ioinsert cache (fileName) (fileContent)
                  let decryptResponse = encryptDecrypt sessionKey encryptedResponse
                  putStrLn decryptResponse
                  mainloop session cache
            (_) -> do 
              mainloop session cache
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
                    ("y") -> do
                      let file = File (encryptDecrypt sessionKey fileName) (encryptDecrypt sessionKey fileContent)
                      manager <- newManager defaultManagerSettings
                      res <- runClientM (close (FileUpload ticket encryptedTimeout file)) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
                      case res of
                        Left err -> putStrLn $ "Error: " ++ show err
                        Right (uploadResponse@(Response encryptedResponse)) -> do
                          C.ioinsert cache (fileName) (fileContent)
                          let decryptResponse = encryptDecrypt sessionKey encryptedResponse
                          putStrLn decryptResponse
                          mainloop session cache
                    (_) -> mainloop session cache
  mainloop session cache

{-isTokenValid :: User -> IO()
isTokenValid user = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (isvalidQuery user) (ClientEnv manager (BaseUrl Http authserverhost (read(authserverport) :: Int) ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
   Right responser -> do case (response responser) of
                            "Token is Valid" -> return()
                            _ -> do putStrLn $ "Session timeout, returning to login menu"
                                    authpart

extendToken :: User -> IO()
extendToken user = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (extendQuery user) (ClientEnv manager (BaseUrl Http authserverhost (read(authserverport) :: Int) ""))
  case res of
   Left err -> putStrLn $ "Error: " ++ show err
   Right response -> return()
-}

{-getFile:: String -> Session -> (C.Handle String String) -> IO()
getFile filename session cache = do
  isTokenValid user
  locksuccess <- lockFile filename
  case locksuccess of
    True -> do
             manager <- newManager defaultManagerSettings
             res <- runClientM (openQuery filename) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
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
-}
                                    
{-putFile:: File -> User-> (C.Handle String String) -> IO ()
putFile file user cache = do
  isTokenValid user
  locksuccess <- lockFile (fileName file)
  case locksuccess of
    True -> do manager <- newManager defaultManagerSettings
               res <- runClientM (closeQuery file) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
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

-}
lockFile :: String -> IO Bool
lockFile fName = do 
  manager <- newManager defaultManagerSettings
  res <- runClientM (islockedQuery fName) (ClientEnv manager (BaseUrl Http lockserverhost (read(lockserverport) :: Int) ""))
  case res of
   Left err -> do putStrLn $ "Error: " ++ show err
                  return False
                          
   Right responser -> do case responser of
                           True -> return False
                           False -> do res <- runClientM (lockQuery fName) (ClientEnv manager (BaseUrl Http lockserverhost (read(lockserverport) :: Int) ""))
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

                        
