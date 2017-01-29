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
import Data.List.Split
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
remove :: FileName -> ClientM Response

join :<|> open :<|> close :<|> allfiles :<|> remove = client directoryApi

openQuery:: FileName -> ClientM File
openQuery filename = do
	openquery <- open filename
	return openquery

lockingApi :: Proxy LockingApi
lockingApi = Proxy

lock :: FileName -> ClientM Response
unlock :: FileName -> ClientM Response
islocked :: FileName -> ClientM Response

lock :<|> unlock :<|> islocked = client lockingApi

transactionApi :: Proxy TransactionApi
transactionApi = Proxy

begin :: Ticket -> ClientM Response
transDownload :: FileName -> ClientM File
transUpload :: FileUpload -> ClientM Response
transCommit :: Ticket -> ClientM Response

begin :<|> transDownload :<|> transUpload :<|> transCommit = client transactionApi

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
        putStrLn ("Could not login user")
        authpart
      _ -> do
        putStrLn ("Client Login Successful")
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
    putStrLn $ "Enter one of the following commands: FILES/UPLOAD/DOWNLOAD/TRANSACTION/CLOSE"
    cmd <- getLine
    case cmd of
        "FILES" -> displayFiles session cache
        "UPLOAD" -> uploadFile session cache
        "DOWNLOAD" -> downloadFile session cache
        "TRANSACTION" -> transactionMode session cache
        "CLOSE" -> putStrLn $ "Closing service!"
        _ -> do putStrLn $ "Invalid Command. Try Again"
                mainloop session cache

updateCache :: Session -> (C.Handle String String) -> IO()
updateCache session@(Session ticket sessionKey encryptedTimeout) cache = liftIO $ do
  putStrLn "Updating Cache..."
  fileList <- C.iogetContents cache
  mapM (downloadAndUpdate session cache) fileList
  putStrLn "Cache Updated"


downloadAndUpdate :: Session -> (C.Handle String String) -> String -> IO()
downloadAndUpdate session@(Session ticket sessionKey encryptedTimeout) cache fileName = liftIO $ do
  manager <- newManager defaultManagerSettings
  let encryptedFN = encryptDecrypt sessionKey fileName
  res <- runClientM (openQuery (FileName ticket encryptedTimeout encryptedFN)) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
  case res of
    Left err -> putStrLn (show err)
    Right response -> do 
      let decryptedFC = encryptDecrypt sessionKey (fileContent response)
      C.ioinsert cache fileName decryptedFC
      putStrLn ("File: " ++ fileName ++ " Updated in Cache")



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
    islocked <- lockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
    case islocked of 
      True -> do 
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
            isunlocked <- unlockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
            case isunlocked of
              True -> putStrLn "File unlocked Successfully"
              False -> putStrLn "File cant be unlocked...This should never happen"
            mainloop session cache
      False -> do
        putStrLn "Failed to lock file"
        mainloop session cache


downloadFile :: Session -> (C.Handle String String) -> IO()
downloadFile session@(Session ticket sessionKey encryptedTimeout) cache = do
  updateCache session cache
  putStrLn "Please enter the name of the file to download"
  fileName <- getLine
  islocked <- lockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
  case islocked of 
    True -> do 
      incache <- C.iolookup cache fileName
      case incache of
        (Nothing) -> do
          manager <- newManager defaultManagerSettings
          let encryptedFN = encryptDecrypt sessionKey fileName
          res <- runClientM (openQuery (FileName ticket encryptedTimeout encryptedFN)) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
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
                      isunlocked <- unlockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
                      case isunlocked of
                        True -> putStrLn "File unlocked Successfully"
                        False -> putStrLn "File cant be unlocked...This should never happen"
                      mainloop session cache
                (_) -> do
                  isunlocked <- unlockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
                  case isunlocked of
                        True -> putStrLn "File unlocked Successfully"
                        False -> putStrLn "File cant be unlocked...This should never happen"
                  mainloop session cache
        (Just v) -> do putStrLn $ "Cache hit"
                       liftIO (writeFile (fileName) v)
                       let cmd = shell ("vim " ++ fileName)
                       createProcess_ "vim" cmd
                       putStrLn $ "When finished please press enter"
                       yesorno <- getLine
                       putStrLn $ "Would you like to upload changes y/n?"
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
                              isunlocked <- unlockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
                              case isunlocked of
                                True -> putStrLn "File unlocked Successfully"
                                False -> putStrLn "File cant be unlocked...This should never happen"
                              mainloop session cache
                        (_) -> do 
                          isunlocked <- unlockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
                          case isunlocked of
                            True -> putStrLn "File unlocked Successfully"
                            False -> putStrLn "File cant be unlocked...This should never happen"
                          mainloop session cache
    False -> do putStrLn "Locking Failed"
                mainloop session cache

transactionMode :: Session -> (C.Handle String String) -> IO()
transactionMode session@(Session ticket sessionKey encryptedTimeout) cache = liftIO $ do
  manager <- newManager defaultManagerSettings
  res <- runClientM (begin (Ticket ticket encryptedTimeout)) (ClientEnv manager (BaseUrl Http transserverhost (read(transserverport) :: Int) ""))
  case res of
    Left err -> putStrLn (show err)
    Right (Response response) -> do
      case (encryptDecrypt sessionKey response) of
        "Successful" -> do
          putStrLn ("Transaction started")
          putStrLn "Fetching file list. Please wait." 
          res <- runClientM (allfiles (Ticket ticket encryptedTimeout)) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
          case res of
           Left err -> putStrLn $ "Error: " ++ show err
           Right fileEncrypt -> do 
            let decryptedFiles = encryptDecryptArray sessionKey fileEncrypt
            mapM putStrLn decryptedFiles
            putStrLn "Please enter the names of the files you wish to download seperated by a comma:"
            fileNames <- getLine
            let clines = splitOn "," fileNames
            lock <- mapM (\s -> lockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey s)) sessionKey) clines
            let are_locked = listand lock
            case are_locked of
              True -> do
                createDirectoryIfMissing True ("tmp/")
                setCurrentDirectory ("tmp/")
                mapM (transactionProcess session cache) clines
                putStrLn "All files processed. Press enter to commit the changes"
                enter <- getLine
                res <- runClientM (transCommit (Ticket ticket encryptedTimeout)) (ClientEnv manager (BaseUrl Http transserverhost (read(transserverport) :: Int) ""))
                case res of
                  Left err -> do 
                    putStrLn (show err)
                    mainloop session cache
                  Right (Response response) -> do
                    putStrLn ("File commit - " ++ (encryptDecrypt sessionKey response))
                    setCurrentDirectory ("../")
                    removeDirectoryRecursive ("tmp/")
                    mapM (\s -> unlockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey s)) sessionKey ) clines
                    mainloop session cache
              False -> do
                putStrLn "Could not lock all files requested"
                mainloop session cache
        _ -> do
               putStrLn "Transaction Failed"


transactionProcess :: Session -> (C.Handle String String) -> String -> IO()
transactionProcess session@(Session ticket sessionKey encryptedTimeout) cache fileName = liftIO $ do
  putStrLn ("Beginning transaction for file: " ++ fileName)
  manager <- newManager defaultManagerSettings
  res <- runClientM (transDownload (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName))) (ClientEnv manager (BaseUrl Http transserverhost (read(transserverport) :: Int) ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err                 
    Right response -> do 
      let decryptedFC = encryptDecrypt sessionKey (fileContent response)
      liftIO (writeFile fileName decryptedFC)
      let cmd = shell ("vim " ++ fileName)
      createProcess_ "vim" cmd
      putStrLn $ "When finished please press Enter"
      yesorno <- getLine
      putStrLn $ "Uploading changes"
      fileContent <- readFile fileName
      let file = File (encryptDecrypt sessionKey fileName) (encryptDecrypt sessionKey fileContent)
      manager <- newManager defaultManagerSettings
      res <- runClientM (transUpload (FileUpload ticket encryptedTimeout file)) (ClientEnv manager (BaseUrl Http transserverhost (read(transserverport) :: Int) ""))
      case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (uploadResponse@(Response encryptedResponse)) -> do
          let decryptResponse = encryptDecrypt sessionKey encryptedResponse
          C.ioinsert cache (fileName) (fileContent)
          putStrLn ("File: " ++ fileName ++ " " ++ decryptResponse)
          --isunlocked <- unlockFile (FileName ticket encryptedTimeout (encryptDecrypt sessionKey fileName)) sessionKey
          --case isunlocked of
          --  True -> putStrLn "File unlocked Successfully"
          --  False -> putStrLn "File cant be unlocked...This should never happen"
          --    mainloop session cache













lockFile :: FileName -> String -> IO Bool
lockFile fName sessionKey = do 
  manager <- newManager defaultManagerSettings
  res <- runClientM (lock fName) (ClientEnv manager (BaseUrl Http lockserverhost (read(lockserverport) :: Int) ""))
  case res of
    Left err -> do putStrLn $ "Error: " ++ show err
                   return False
    Right (Response response) -> do 
      putStrLn (encryptDecrypt sessionKey response)
      case (encryptDecrypt sessionKey response) of
        "Successful" -> return True
        _ -> return False


unlockFile :: FileName -> String -> IO Bool
unlockFile fName sessionKey = do 
  manager <- newManager defaultManagerSettings
  res <- runClientM (unlock fName) (ClientEnv manager (BaseUrl Http lockserverhost (read(lockserverport) :: Int) ""))
  case res of
    Left err -> do putStrLn $ "Error: " ++ show err
                   return False
    Right (Response response) -> do
     case (encryptDecrypt sessionKey response) of
        "Successful" -> return True
        _ -> return False

listand     ::  [Bool] -> Bool
listand []              = True
listand (x:xs)  
    | x == False        = False
    | otherwise         = listand xs
                        
