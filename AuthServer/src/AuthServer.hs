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

module AuthServer where
import System.Random
import           Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.List.Split
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
import           	Data.Maybe                   (catMaybes, fromJust)
import           	Data.Text                    (pack, unpack)
import           	Data.Time.Clock              (UTCTime, getCurrentTime)
import           	Data.Time.Format             (defaultTimeLocale, formatTime)
import           	Database.MongoDB 
import Control.Monad (when)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.UUID.V1
import Data.UUID hiding (null)
import Data.Time
import CommonResources
import MongodbHelpers

type ApiHandler = ExceptT ServantErr IO


authApi :: Proxy AuthApi
authApi = Proxy

server :: Server AuthApi
server = 
    login :<|>
    newuser :<|>
    checkToken :<|>
    extendToken

authApp :: Application
authApp = serve authApi server

mkApp :: IO()
mkApp = do
    run (read (authserverport) ::Int) authApp 

login :: Signin -> ApiHandler User
login signin = liftIO $ do
	let uname = susername signin
	let psswrd = spassword signin
	warnLog $ "Searching for value for key: " ++ uname ++ ", " ++ psswrd
	user <- withMongoDbConnection $ do 
	    docs <- find (select ["_id" =: (uname++psswrd)] "USER_RECORD") >>= drainCursor
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Signin) docs
        let thisuser = head $ user
        putStrLn (show thisuser)
        a <- nextUUID
        let tokener = Data.UUID.toString $ fromJust a
        currentTime <- getCurrentTime
        currentZone <- getCurrentTimeZone
        let fiveMinutes = 5 * 60
        let newTime = addUTCTime fiveMinutes currentTime
        let timeouter = utcToLocalTime currentZone newTime
        let finaltimeout = show timeouter
        warnLog $ "Storing Session key under key " ++ uname ++ "."
        let session = (User (susername thisuser) (spassword thisuser) finaltimeout tokener)
	withMongoDbConnection $ upsert (select ["_id" =: uname] "SESSION_RECORD") $ toBSON session
        warnLog $ "Session Successfully Stored."
	return (session)


newuser :: Signin -> ApiHandler Response
newuser signin = liftIO $ do
  let uname = susername signin
  let psswrd = spassword signin
  warnLog $ "Storing value under key: " ++ uname
  let user = (Signin uname psswrd)
  withMongoDbConnection $ upsert (select ["_id" =: (uname++psswrd)] "USER_RECORD") $ toBSON user
  return (Response "Success")

checkToken :: User -> ApiHandler Response
checkToken user = liftIO $ do
  let uname = uusername user
  let tokener = token user
  warnLog $ "Searching for user: " ++ uname
  session <- withMongoDbConnection $ do 
      docs <- find (select ["_id" =: uname] "SESSION_RECORD") >>= drainCursor
      return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe User) docs
  warnLog $ "User found" ++ (show session)
  let thissession = head $ session
  let senttoken = token user
  let storedtoken = token thissession
  case senttoken == storedtoken of
      False -> return (Response "Token not valid")
      True -> do let tokentimeout = timeout thissession
                 currentTime <- getCurrentTime
                 currentZone <- getCurrentTimeZone
                 let localcurrentTime = show (utcToLocalTime currentZone currentTime)
                 let tokentimeoutsplit = splitOn " " $ tokentimeout
                 let localcurrentTimesplit = splitOn " " $ localcurrentTime
                 case ((tokentimeoutsplit !! 0) == (localcurrentTimesplit !! 0)) of
                      False -> return (Response "Current date not equal to token date")
                      True -> do let localcurrenthours = localcurrentTimesplit !! 1
                                 let tokenhours = tokentimeoutsplit !! 1
                                 let localhour = read(((splitOn ":" $ localcurrenthours) !! 0))
                                 let tokenhour = read(((splitOn ":" $ tokenhours) !! 0))
                                 let tokenminutes = read(((splitOn ":" $ tokenhours) !! 1))
                                 let currentminutes = read(((splitOn ":" $ localcurrenthours) !! 1))
                                 let totaltokenminutes = (tokenhour * 60) + tokenminutes
                                 let totalcurrentminutes = (localhour * 60) + currentminutes
                                 case totaltokenminutes > totalcurrentminutes of
                                      False -> return (Response "Token Timeout")
                                      True -> return (Response "Token is Valid")


extendToken :: User -> ApiHandler Response
extendToken user = liftIO $ do
  currentTime <- getCurrentTime
  currentZone <- getCurrentTimeZone
  let fiveMinutes = 5 * 60
  let newTime = addUTCTime fiveMinutes currentTime
  let timeouter = utcToLocalTime currentZone newTime
  let finaltimeout = show timeouter
  warnLog $ "Storing Session key under key " ++ (uusername user) ++ "."
  let session = (User (uusername user) (upassword user) finaltimeout (token user))
  withMongoDbConnection $ upsert (select ["_id" =: (uusername user)] "SESSION_RECORD") $ toBSON session
  warnLog $ "Session Successfully Stored."
  return (Response "Success")


 -- | Logging stuff
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
