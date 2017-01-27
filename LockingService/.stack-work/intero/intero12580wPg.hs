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

module LockingService where
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


lockingApi :: Proxy LockingApi
lockingApi = Proxy

server :: Server LockingApi
server = 
    lockFile :<|>
    unlockFile :<|>
    isFileLocked

lockingApp :: Application
lockingApp = serve lockingApi server

mkApp :: IO()
mkApp = do
  putStrLn ("Starting Lock Service on port: " ++ lockserverport)
  run (read (lockserverport) ::Int) lockingApp


lockFile :: FileName -> ApiHandler Bool
lockFile (FileName ticket encryptedTimeout encryptedFN) = liftIO $ do
  let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket
  let decryptedFN = encryptDecrypt sessionKey encryptedFN

  putStrLn ("Checking Client Credentials...")

  currentTime <- getCurrentTime

  if (currentTime > decryptedTimeout) then do
    putStrLn "Client session timeout"
    return (Response (encryptDecrypt sessionKey "Failed"))

  else do
  	putStrLn ("Locking File" ++ decryptedFN)
  	locks <- withMongoDbConnection $ do
          docs <- find (select ["_id" =: decryptedFN] "LOCK_RECORD") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case locks of
          [(Lock _ True)] -> return (Response (encryptDecrypt sessionKey "Failed"))
          [(Lock _ False)] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: decryptedFN] "LOCK_RECORD") $ toBSON $ (Lock decryptedFN True)
                                          return (Response (encryptDecrypt sessionKey "Successful"))
          [] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: decryptedFN] "LOCK_RECORD") $ toBSON $ (Lock decryptedFN True)
                            return (Response (encryptDecrypt sessionKey "Successful"))

unlockFile :: FileName -> ApiHandler Response
unlockFile (FileName ticket encryptedTimeout encryptedFN) = liftIO $ do
  let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket
  let decryptedFN = encryptDecrypt sessionKey encryptedFN

  putStrLn ("Checking Client Credentials...")

  currentTime <- getCurrentTime

  if (currentTime > decryptedTimeout) then do
    putStrLn "Client session timeout"
    return (Response (encryptDecrypt sessionKey "SessionTimeout"))

  else do
  	putStrLn ("Unlocking File" ++ decryptedFN)
  	locks <- liftIO $ withMongoDbConnection $ do
          docs <- find (select ["_id" =: decryptedFN] "LOCK_RECORD") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case locks of
          [(Lock _ False)] -> return (Response (encryptDecrypt sessionKey "Failed"))
          [(Lock _ True)] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: decryptedFN] "LOCK_RECORD") $ toBSON $ (Lock decryptedFN False)
                                         return (Response (encryptDecrypt sessionKey "Successful"))
          [] -> liftIO $ do withMongoDbConnection $ upsert (select ["_id" =: decryptedFN] "LOCK_RECORD") $ toBSON $ (Lock decryptedFN False)
                            return (Response (encryptDecrypt sessionKey "Successful"))

isFileLocked :: FileName -> ApiHandler Response
isFileLocked (FileName ticket encryptedTimeout encryptedFN) = liftIO $ do
  let decryptedTimeout = decryptTime sharedSecret encryptedTimeout
  let sessionKey = encryptDecrypt sharedSecret ticket
  let decryptedFN = encryptDecrypt sessionKey encryptedFN

  putStrLn ("Checking Client Credentials...")

  currentTime <- getCurrentTime

  if (currentTime > decryptedTimeout) then do
    putStrLn "Client session timeout"
    return (Response (encryptDecrypt sessionKey "Failed"))

  else do
  	putStrLn ("Checking is file" ++ decryptedFN ++ "is locked")
  	locks <- liftIO $ withMongoDbConnection $ do
          docs <- find (select ["_id" =: decryptedFN] "LOCK_RECORD") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case locks of
          [(Lock _ True)] -> return (Response (encryptDecrypt sessionKey "Locked"))
          _ -> return (Response (encryptDecrypt sessionKey "Unlocked"))


{-} -- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger
-}
