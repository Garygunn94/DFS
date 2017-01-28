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

module FileServer where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai hiding(Response)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           System.Directory
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           CommonResources
import           Data.Time


type ApiHandler = ExceptT ServantErr IO


directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

join :: FileServer -> ClientM Response
open :: FileName -> ClientM File
close :: FileUpload -> ClientM Response
allfiles :: Ticket -> ClientM [String]
remove :: FileName -> ClientM Response

join :<|> open :<|> close :<|> allfiles :<|> remove = client directoryApi

joinQuery :: FileServer -> ClientM Response
joinQuery fs = do
  get_join <- join fs
  return get_join
  
fileApi :: Proxy FileApi
fileApi = Proxy

server :: Server FileApi
server = 
    getFiles :<|>
    downloadFile :<|>
    uploadFile :<|>
    deleteFile

fileApp :: Application
fileApp = serve fileApi server

mkApp :: IO()
mkApp = do
   -- fsToDsHandshake
    createDirectoryIfMissing True ("fileserver" ++ fsserverhost ++ ":" ++ fsserverport ++ "/")
    setCurrentDirectory ("fileserver" ++ fsserverhost ++ ":" ++ fsserverport ++ "/")
    putStrLn $ "Attempting to join directory server"
    manager <- newManager defaultManagerSettings
    let fs = (FileServer (fsserverhost++fsserverport) fsserverhost fsserverport)
    res <- runClientM (joinQuery fs) (ClientEnv manager (BaseUrl Http dirserverhost (read(dirserverport) :: Int) ""))
    case res of
       Left err -> putStrLn $ "Error: " ++ show err
       Right response -> do
        putStrLn "Service Joined Successfully"
        putStrLn ("Starting FileServer on port: " ++ fsserverport)
        run (read (fsserverport) ::Int) fileApp 

getFiles :: ApiHandler [FilePath]
getFiles =
  liftIO(getDirectoryContents ("../fileserver" ++ fsserverhost ++ ":" ++ fsserverport ++ "/"))

downloadFile :: FileName -> ApiHandler File
downloadFile (FileName ticket encryptedTimeout encryptedFN) = do
  let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket
  let decryptedFN = encryptDecrypt sessionKey encryptedFN  

  currentTime <- liftIO $ getCurrentTime
  if (currentTime > decryptedTimeout) then do
    liftIO $ putStrLn ("Client ticket timeout")
    let encryptedResponse = encryptDecrypt sessionKey ("Session Timeout")
    return (File encryptedFN encryptedResponse)
  else do
    liftIO $ putStrLn "Fetching File Contents"
    content <- liftIO (readFile decryptedFN)
    return (File encryptedFN (encryptDecrypt sessionKey content))

uploadFile :: FileUpload -> ApiHandler Response
uploadFile (FileUpload ticket encryptTimeout (File encryptedFN encryptedFC)) = do
  let decryptedTimeout = decryptTime sharedSecret encryptTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket
  let decryptedFN = encryptDecrypt sessionKey encryptedFN

  currentTime <- liftIO $ getCurrentTime
  if (currentTime > decryptedTimeout) then do
    liftIO $ putStrLn ("Client ticket timeout")
    let encryptedResponse = encryptDecrypt sessionKey ("SessionTimeout")
    return (Response encryptedResponse)
  else do
    let decryptedFC = encryptDecrypt sessionKey encryptedFC
    liftIO $ putStrLn ("Storing File")
    liftIO (writeFile decryptedFN decryptedFC)
    let encryptedResponse = encryptDecrypt sessionKey "Success"
    return (Response encryptedResponse)

deleteFile :: FileName -> ApiHandler Response
deleteFile (FileName ticket encryptedTimeout encryptedFN) = liftIO $ do
  let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket
  let decryptedFN = encryptDecrypt sessionKey encryptedFN  

  currentTime <- liftIO $ getCurrentTime
  if (currentTime > decryptedTimeout) then do
    putStrLn ("Client ticket timeout")
    let encryptedResponse = encryptDecrypt sessionKey ("Session Timeout")
    return (Response (encryptDecrypt sessionKey "SessionTimeout"))
  else do
    putStrLn ("Deleting file: " ++ decryptedFN)
    removeFile decryptedFN
    return (Response (encryptDecrypt sessionKey "Success"))

