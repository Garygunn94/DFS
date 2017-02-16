
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

module TransactionServer where
import System.Random
import           Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Network.Wai hiding(Response)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import           System.Directory
import           	System.Environment           (getArgs, getProgName, lookupEnv)
import           	System.Log.Formatter
import           	System.Log.Handler           (setFormatter)
import           	System.Log.Handler.Simple
import           	System.Log.Handler.Syslog
import           	System.Log.Logger
import         	  Data.Bson.Generic
import qualified	Data.List                    as DL
import           	Data.Maybe                   (catMaybes)
import           	Data.Text                    (pack, unpack)
import           	Data.Time.Clock              (UTCTime, getCurrentTime)
import           	Data.Time.Format             (defaultTimeLocale, formatTime)
import           	Database.MongoDB 
import Control.Monad (when)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import CommonResources
import MongodbHelpers

type ApiHandler = ExceptT ServantErr IO

transactionApi :: Proxy TransactionApi
transactionApi = Proxy

server :: Server TransactionApi
server = 
    beginTrans :<|>
    downloadTrans :<|>
    uploadTrans :<|>
    commitTrans

transactionApp :: Application
transactionApp = serve transactionApi server

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

join :: FileServer -> ClientM Response
open :: FileName -> ClientM File
close :: FileUpload -> ClientM Response
allfiles :: Ticket -> ClientM [String]
remove :: FileName -> ClientM Response

join :<|> open :<|> close :<|> allfiles :<|> remove = client directoryApi

runApp :: IO()
runApp = do
    putStrLn ("Starting TransactionServer on port: " ++ transserverport)
    run (read (transserverport) ::Int) transactionApp

beginTrans :: Ticket -> ApiHandler Response
beginTrans (Ticket ticket encryptedTimeout) = liftIO $ do
	let sessionKey = encryptDecrypt sharedSecret ticket
	let decryptedTimeout = decryptTime sharedSecret encryptedTimeout

	putStrLn ("Checking Client Credentials...")
	currentTime <- getCurrentTime
        if (currentTime > decryptedTimeout) then do
          putStrLn "Client session timeout"
          return (Response (encryptDecrypt sessionKey "Failed"))

        else do
    	  putStrLn "Starting transaction"
    	  putStrLn "Storing client sessionKey as transaction ID"
    	  withMongoDbConnection $ upsert (select ["transactionID" =: sessionKey] "TRANSACTION_ID_RECORD") $ toBSON sessionKey
    	  return (Response (encryptDecrypt sessionKey "Successful"))

downloadTrans :: FileName -> ApiHandler File
downloadTrans fileName@(FileName ticket encryptedTimeout encryptedFN) = liftIO $ do
	let sessionKey = encryptDecrypt sharedSecret ticket
	let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
	let decryptedFN = encryptDecrypt sessionKey encryptedFN

	putStrLn ("Checking Client Credentials...")

        currentTime <- getCurrentTime

        if (currentTime > decryptedTimeout) then do
          putStrLn "Client session timeout"
          return (File "Failed" "Failed")

        else do
          manager <- newManager defaultManagerSettings
          res <- runClientM (open fileName) (ClientEnv manager (BaseUrl Http dirserverhost (read (dirserverport) :: Int)  "")) 
          case res of
        	Left err -> do 
        		putStrLn (show err)
        		return (File "Failed" "Failed")
        	Right file -> do
        		putStrLn "Storing file transaction data"
        		withMongoDbConnection $ upsert (select ["userID" =: sessionKey, "transFileName" =: decryptedFN] "TRANSACTION_FILE_RECORD") $ toBSON (TransactionFile decryptedFN sessionKey)
        		return file

uploadTrans :: FileUpload -> ApiHandler Response
uploadTrans fileUpload@(FileUpload ticket encryptedTimeout (File encryptedFN encryptedFC)) = liftIO $ do
	let sessionKey = encryptDecrypt sharedSecret ticket
	let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
	let decryptedFN = encryptDecrypt sessionKey encryptedFN

	putStrLn ("Checking Client Credentials...")

        currentTime <- getCurrentTime

        if (currentTime > decryptedTimeout) then do
          putStrLn "Client session timeout"
          return (Response (encryptDecrypt sessionKey "Failed"))

        else do
    	  manager <- newManager defaultManagerSettings
    	  let tempFileName = encryptDecrypt sessionKey ("TMP~"++decryptedFN)
    	  let fupload = FileUpload ticket encryptedTimeout (File tempFileName encryptedFC)

    	  res <- runClientM (TransactionServer.close fupload) (ClientEnv manager (BaseUrl Http dirserverhost (read (dirserverport) :: Int)  ""))
    	  case res of
    		Left err -> do
    			putStrLn (show err)
    			return (Response (encryptDecrypt sessionKey "Failed"))
    		Right (Response response) -> do
    			let decryptedres = encryptDecrypt sessionKey response
    			putStrLn ("Uploaded temp file - " ++ decryptedres)
    			return (Response response)

commitTrans :: Ticket -> ApiHandler Response
commitTrans tic@(Ticket ticket encryptedTimeout) = liftIO $ do
	let sessionKey = encryptDecrypt sharedSecret ticket
	let decryptedTimeout = decryptTime sharedSecret encryptedTimeout

	putStrLn ("Checking Client Credentials...")

        currentTime <- getCurrentTime

        if (currentTime > decryptedTimeout) then do
          putStrLn "Client session timeout"
          return (Response (encryptDecrypt sessionKey "Failed"))

        else do
    	  transactions <- liftIO $ withMongoDbConnection $ do
            docs <- find (select ["userID" =: sessionKey] "TRANSACTION_FILE_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransactionFile) docs
          mapM (commitfile tic) transactions
          liftIO $ withMongoDbConnection $ do
            delete (select ["userID" =: sessionKey] "TRANSACTION_FILE_RECORD")
          liftIO $ withMongoDbConnection $ do
            delete (select ["transactionID" =: sessionKey] "TRANSACTION_ID_RECORD")
          return (Response (encryptDecrypt sessionKey "Successful"))

commitfile :: Ticket -> TransactionFile -> IO()
commitfile (Ticket ticket encryptedTimeout) (TransactionFile decryptedFN sessionKey) = liftIO $ do
	putStrLn ("Commiting file: " ++ decryptedFN)
	manager <- newManager defaultManagerSettings
	let temp_file = encryptDecrypt sessionKey ("TMP~"++ decryptedFN)
	let fileName = (FileName ticket encryptedTimeout temp_file)
        res <- runClientM (open fileName) (ClientEnv manager (BaseUrl Http dirserverhost (read (dirserverport) :: Int)  "")) 
        case res of
    	  Left err -> putStrLn (show err)
    	  Right (File encryptedFN encryptedFC) -> do
            let fn = encryptDecrypt sessionKey encryptedFN
            let temp = encryptDecrypt sessionKey temp_file
            case (temp == fn) of
    			False -> putStrLn "Commit Failed"
    			True -> do
    				let fileupload = (FileUpload ticket encryptedTimeout (File (encryptDecrypt sessionKey decryptedFN) encryptedFC))
    				res <- runClientM (TransactionServer.close fileupload) (ClientEnv manager (BaseUrl Http dirserverhost (read (dirserverport) :: Int)  ""))
    				case res of
    					Left err -> do putStrLn (show err)
    					Right (Response response) -> do
                                                let uploadresponse = encryptDecrypt sessionKey response
                                                putStrLn uploadresponse
    						case uploadresponse of
    							"Success" -> do
    								res <- runClientM (remove (FileName ticket encryptedTimeout temp_file)) (ClientEnv manager (BaseUrl Http dirserverhost (read (dirserverport) :: Int)  "")) 
    								case res of
    									Left err -> putStrLn (show err)
    									Right (Response response) -> putStrLn (encryptDecrypt sessionKey response)
                                        
                                                        _ -> putStrLn "Shouldnt get here"












