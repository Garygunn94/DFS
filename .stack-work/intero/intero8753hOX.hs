{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}


module DirectoryServer where
import Network hiding (accept, sClose)
import Network.Socket hiding (send, recv, sendTo, recvFrom, Broadcast)
import Network.Socket.ByteString
import Data.ByteString.Char8 (pack, unpack)
import System.Environment
import System.IO
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, join)
import Data.List.Split
import Data.Word
import Text.Printf (printf)
import System.Directory
import Data.Map (Map) -- from the `containers` library
import Data.Time
import System.Random
import qualified Data.Map as M
import LRUCache as C
import Data.Hashable


type Uuid = Int
type Address = String
type Port = String
type Filename = String
type Timestamp = IO String

--Server data type allows me to pass address and port details easily
data DirectoryServer = DirectoryServer
  { address :: String
  , port :: String
  , filemappings   :: TVar (M.Map Filename Filemapping)
  , fileservers :: TVar (M.Map Uuid Fileserver)
  , fileservercount :: TVar Int
}

--Constructor
newDirectoryServer :: String -> String -> IO DirectoryServer
newDirectoryServer address port = atomically $ do DirectoryServer <$> return address <*> return port <*> newTVar M.empty <*> newTVar M.empty <*> newTVar 0 

addFilemapping :: DirectoryServer -> Filename -> Uuid -> Address -> Port -> Timestamp -> STM ()
addFilemapping DirectoryServer{..} filename uuid fmaddress fmport timestamp = do
    fm <- newFilemapping filename uuid fmaddress fmport timestamp
    modifyTVar filemappings . M.insert filename $ fm

addFileserver :: DirectoryServer -> Uuid -> Address -> Port -> STM ()
addFileserver DirectoryServer{..} uuid fsaddress fsport = do
    fs <- newFileserver uuid fsaddress fsport
    modifyTVar fileservers . M.insert uuid $ fs

lookupFilemapping :: DirectoryServer -> Filename -> STM (Maybe Filemapping)
lookupFilemapping DirectoryServer{..} filename = M.lookup filename <$> readTVar filemappings

lookupFileserver :: DirectoryServer -> Uuid -> STM (Maybe Fileserver)
lookupFileserver DirectoryServer{..} uuid = M.lookup uuid <$> readTVar fileservers

data Filemapping = Filemapping
  { fmfilename :: Filename
  , fmuuid     :: Uuid
  , fmaddress  :: Address
  , fmport     :: Port
  , fmtimestamp :: Timestamp
  }

newFilemapping :: Filename -> Uuid -> Address -> Port -> Timestamp -> STM Filemapping
newFilemapping fmfilename fmuuid fmaddress fmport fmtimestamp = Filemapping <$> return fmfilename <*> return fmuuid <*> return fmaddress <*> return fmport <*> return fmtimestamp

getFilemappinguuid :: Filemapping -> Uuid
getFilemappinguuid Filemapping{..} = fmuuid

getFilemappingaddress :: Filemapping -> Address
getFilemappingaddress Filemapping{..} = fmaddress

getFilemappingport :: Filemapping -> Port
getFilemappingport Filemapping{..} = fmport

getFilemappingtimestamp :: Filemapping -> Timestamp
getFilemappingtimestamp Filemapping{..} = fmtimestamp

data Fileserver = Fileserver
  { fsuuid :: Uuid
  , fsaddress  :: HostName
  , fsport     :: Port
  }

newFileserver :: Uuid -> Address -> Port -> STM Fileserver
newFileserver fsuuid fsaddress fsport = Fileserver <$> return fsuuid <*> return fsaddress <*> return fsport

getFileserveraddress :: Fileserver -> HostName
getFileserveraddress Fileserver{..} = fsaddress

getFileserverport :: Fileserver -> Port
getFileserverport Fileserver{..} = fsport
--4 is easy for testing the pooling
maxnumThreads = 4
serverport :: String
serverport = "7008"

serverhost :: String
serverhost = "localhost"

dirrun:: IO ()
dirrun = withSocketsDo $ do
  --Command line arguments for port and address
  --args <- getArgs
  server <- newDirectoryServer serverhost serverport
    --sock <- listenOn (PortNumber (fromIntegral serverport))

  addrinfos <- getAddrInfo
			 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
			 Nothing (Just serverport)

  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 5

  _ <- printf "Listening on port %s\n" serverport
  --Listen on port from command line argument
  
  --New Abstract FIFO Channel
  chan <- newChan

  --New Cache
  cache <- C.newHandle 5
  
  --Tvars are variables Stored in memory, this way we can access the numThreads from any method
  numThreads <- atomically $ newTVar 0

  --Spawns a new thread to handle the clientconnectHandler method, passes socket, channel, numThreads and server
  forkIO $ clientconnectHandler sock chan numThreads server cache
  
  --Calls the mainHandler which will monitor the FIFO channel
  mainHandler sock chan

mainHandler :: Socket -> Chan String -> IO ()
mainHandler sock chan = do

  --Read current message on the FIFO channel
  chanMsg <- readChan chan

  --If KILL_SERVICE, stop mainHandler running, If anything else, call mainHandler again, keeping the service running
  case (chanMsg) of
    ("KILL_SERVICE") -> putStrLn "Terminating the Service!"
    _ -> mainHandler sock chan

clientconnectHandler :: Socket -> Chan String -> TVar Int -> DirectoryServer  -> (C.Handle String String) -> IO ()
clientconnectHandler sock chan numThreads server cache = do

  --Accept the socket which returns a handle, host and port
  --(handle, host, port) <- accept sock
  (s,a) <- accept sock
  --handle <- socketToHandle s ReadWriteMode
  --Read numThreads from memory and print it on server console
  count <- atomically $ readTVar numThreads
  putStrLn $ "numThreads = " ++ show count

  --If there are still threads remaining create new thread and increment (exception if thread is lost -> decrement), else tell user capacity has been reached
  if (count < maxnumThreads) then do
    forkFinally (clientHandler s chan server cache) (\_ -> atomically $ decrementTVar numThreads)
    atomically $ incrementTVar numThreads
    else do
      send s (pack ("Maximum number of threads in use. try again soon"++"\n\n"))
      sClose s

  clientconnectHandler sock chan numThreads server cache

clientHandler :: Socket -> Chan String -> DirectoryServer -> (C.Handle String String) -> IO ()
clientHandler sock chan server@DirectoryServer{..} cache =
    forever $ do
        message <- recv sock 1024
	let msg = unpack message
        print $ msg ++ "!ENDLINE!"
        let cmd = head $ words $ head $ splitOn ":" msg
        print cmd
        case cmd of
            ("HELO") -> heloCommand sock server $ (words msg) !! 1
            ("KILL_SERVICE") -> killCommand chan sock
            ("DOWNLOAD") -> downloadCommand sock server msg cache
            ("UPLOAD") -> uploadCommand sock server msg
            ("JOIN") -> joinCommand sock server msg
            ("UPDATE") -> updateCommand sock server msg
            _ -> do send sock (pack ("Unknown Command - " ++ msg ++ "\n\n")) ; return ()

--Function called when HELO text command recieved  
heloCommand :: Socket -> DirectoryServer -> String -> IO ()
heloCommand sock DirectoryServer{..} msg = do
  send sock $ pack $ "HELO " ++ msg ++ "\n" ++
                     "IP:" ++ "192.168.6.129" ++ "\n" ++
                     "Port:" ++ port ++ "\n" ++
                     "StudentID:12306421\n\n"

  return ()

killCommand :: Chan String -> Socket -> IO ()
killCommand chan sock = do
    send sock $ pack $ "Service is now terminating!"
    writeChan chan "KILL_SERVICE"

returnb :: a -> IO a
returnb a = return a

downloadCommand ::  Socket -> DirectoryServer -> String -> (C.Handle String String) -> IO ()
downloadCommand sock server@DirectoryServer{..} command cache = do
  let clines = splitOn "\\n" command
      filename = (splitOn ":" $ clines !! 1) !! 1
  
  -- let k = filename
  let k = filename
  incache <- C.iolookup cache k
  case incache of
    (Nothing) -> do
                      fm <- atomically $ lookupFilemapping server filename
                      case fm of
                         (Nothing) ->  send sock $ pack $ "DOWNLOAD: " ++ filename ++ "\n" ++
                                                          "STATUS: " ++ "File not found" ++ "\n\n"
                         (Just fm) -> do forkIO $ downloadmsg filename (getFilemappingaddress fm) (getFilemappingport fm) sock cache
                                         send sock $ pack $ "DOWNLOAD: " ++ filename ++ "\n" ++
                                                            "STATUS: " ++ "SUCCESSFUL" ++ "\n\n"
    (Just v) -> do    print "Cache hit"
                      ioinsert cache filename v
                      send sock $ pack $ "DOWNLOAD: " ++ filename ++ "\n" ++
                                         "DATA: " ++ show v ++ "\n\n"
      
  return ()
                

downloadmsg :: String -> String -> String -> Socket -> (C.Handle String String) -> IO()
downloadmsg filename host port sock cache = do
  addrInfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "7007")
  let serverAddr = head addrInfo
  clsock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect clsock (addrAddress serverAddr)
  send clsock $ pack $ "DOWNLOAD:FILE" ++ "\\n" ++
                       "FILENAME:" ++ filename ++ "\\n\n"
  resp <- recv clsock 1024
  let msg = unpack resp
  let clines = splitOn "\\n" msg
      fdata = (splitOn ":" $ clines !! 1) !! 1

  sClose clsock
  send sock $ pack $ "DOWNLOAD: " ++ filename ++ "\n" ++
                     "DATA: " ++ fdata ++ "\n\n"

  ioinsert cache filename fdata
  return ()


returndata :: String -> Socket -> String -> IO ()
returndata filename sock fdata = do
  send sock $ pack $ "DOWNLOAD: " ++ filename ++ "\\n" ++
                     "DATA: " ++ fdata ++ "\n\n"
  return ()
  
uploadCommand :: Socket -> DirectoryServer ->String -> IO ()
uploadCommand sock server@DirectoryServer{..} command = do
  let clines = splitOn "\\n" command
      filename = (splitOn ":" $ clines !! 1) !! 1
      fdata = (splitOn ":" $ clines !! 2) !! 1

  fm <- atomically $ lookupFilemapping server filename
  case fm of
    (Just fm) -> send sock $ pack $ "UPLOAD: " ++ filename ++ "\n" ++
                                  "STATUS: " ++ "File Already Exists" ++ "\n\n"
    (Nothing) -> do numfs <- atomically $ M.size <$> readTVar fileservers
                    rand <- randomRIO (0, (numfs-1))
                    fs <- atomically $ lookupFileserver server rand
                    case fs of
                     (Nothing) -> send sock $ pack $ "UPLOAD: " ++ filename ++ "\n"++
                                                  "FAILED: " ++ "No valid Fileserver found to host" ++ "\n\n"

                     (Just fs) -> do forkIO $ uploadmsg sock filename fdata fs rand server
                                     fm <- atomically $ newFilemapping filename rand (getFileserveraddress fs) (getFileserverport fs) (fmap show getZonedTime)
                                     atomically $ addFilemapping server filename rand (getFileserveraddress fs) (getFileserverport fs) (fmap show getZonedTime)
                                     send sock $ pack $ "UPLOAD: " ++ filename ++ "\\n" ++
                                                     "STATUS: " ++ "Successfull" ++ "\n\n"                                 
  return ()
                

uploadmsg :: Socket -> String -> String -> Fileserver -> Int -> DirectoryServer -> IO ()
uploadmsg sock filename fdata fs rand server@DirectoryServer{..} = withSocketsDo $ do
  addrInfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just (getFileserverport fs))
  let serverAddr = head addrInfo
  clsock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect clsock (addrAddress serverAddr)
  send clsock $ pack $ "UPLOAD:FILE" ++ "\\n" ++
                       "FILENAME:" ++ filename ++ "\\n" ++
                       "DATA:" ++ fdata ++ "\\n"
  resp <- recv clsock 1024
  sClose clsock
  let msg = unpack resp
  print $ msg ++ "!ENDLINE!"
  let clines = splitOn "\\n" msg
      status = (splitOn ":" $ clines !! 1) !! 1
  return ()
     

joinCommand :: Socket -> DirectoryServer ->String -> IO ()
joinCommand sock server@DirectoryServer{..} command = do
  let clines = splitOn "\\n" command
      newaddress = (splitOn ":" $ clines !! 1) !! 1
      newport = (splitOn ":" $ clines !! 2) !! 1

  nodeID <- atomically $ readTVar fileservercount
  fs <- atomically $ newFileserver nodeID newaddress newport
  atomically $ addFileserver server nodeID newaddress newport
  atomically $ incrementFileserverCount fileservercount

  send sock $ pack $ "JOINED DISTRIBUTED FILE SERVICE as fileserver: " ++ (show nodeID) ++ "\n\n"

  return ()

updateCommand :: Socket -> DirectoryServer ->String -> IO ()
updateCommand sock server@DirectoryServer{..} command = do
  let clines = splitOn "\\n" command
      filename = (splitOn ":" $ clines !! 1) !! 1
      fdata = (splitOn ":" $ clines !! 2) !! 1

  fm <- atomically $ lookupFilemapping server filename
  case fm of
    (Nothing) -> send sock $ pack $ "UPDATE: " ++ filename ++ "\n" ++
                                  "STATUS: " ++ "File Doesnt Exists" ++ "\n\n"
    (Just fm) -> do fs <- atomically $ lookupFileserver server (getFilemappinguuid fm)
                    case fs of
                     (Nothing) -> send sock $ pack $ "UPDATE: " ++ filename ++ "\n"++
                                                  "FAILED: " ++ "No valid Fileserver found to host" ++ "\n\n"

                     (Just fs) -> do forkIO $ updatemsg sock filename fdata fs (getFilemappinguuid fm) server
                                     fm <- atomically $ newFilemapping filename (getFilemappinguuid fm) (getFileserveraddress fs) (getFileserverport fs) (fmap show getZonedTime)
                                     atomically $ addFilemapping server filename (getFilemappinguuid fm) (getFileserveraddress fs) (getFileserverport fs) (fmap show getZonedTime)
                                     send sock $ pack $ "UPDATE: " ++ filename ++ "\\n" ++
                                                        "STATUS: " ++ "Successfull" ++ "\n\n"                                 
  return ()
                

updatemsg :: Socket -> String -> String -> Fileserver -> Int -> DirectoryServer -> IO ()
updatemsg sock filename fdata fs rand server@DirectoryServer{..} = withSocketsDo $ do
  addrInfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just (getFileserverport fs))
  let serverAddr = head addrInfo
  clsock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect clsock (addrAddress serverAddr) 
  send clsock $ pack $ "UPDATE:FILE" ++ "\\n" ++
                       "FILENAME:" ++ filename ++ "\\n" ++
                       "DATA:" ++ fdata ++ "\\n"
  resp <- recv clsock 1024
  sClose clsock
  let msg = unpack resp
  print $ msg ++ "!ENDLINE!"
  let clines = splitOn "\\n" msg
      status = (splitOn ":" $ clines !! 1) !! 1
  return ()
      
--Increment Tvar stored in memory i.e. numThreads
incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

--Decrement Tvar stored in memory i.e. numThreads
decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)

incrementFileserverCount :: TVar Int -> STM ()
incrementFileserverCount tv = modifyTVar tv ((+) 1)
