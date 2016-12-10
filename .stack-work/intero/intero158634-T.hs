{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
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
import qualified Data.Map as M


type Uuid = Int
type Address = String
type Port = String
type Filename = String
type Timestamp = String

--Server data type allows me to pass address and port details easily
data DirectoryServer = DirectoryServer
  { address :: String
  , port :: String
  , filemappings   :: TVar (M.Map Filename (Uuid, Address, Port, Timestamp))
  , fileservers :: TVar (M.Map Uuid (Address, Port))
}

--Constructor
newDirectoryServer :: String -> String -> IO DirectoryServer
newDirectoryServer address port = atomically $ do DirectoryServer <$> return address <*> return port <*> newTVar M.empty <*> newTVar M.empty 

--4 is easy for testing the pooling
maxnumThreads = 4
serverport :: String
serverport = "7008"

serverhost :: String
serverhost = "localhost"

run:: IO ()
run = withSocketsDo $ do
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
  
  --Tvars are variables Stored in memory, this way we can access the numThreads from any method
  numThreads <- atomically $ newTVar 0

  --Spawns a new thread to handle the clientconnectHandler method, passes socket, channel, numThreads and server
  forkIO $ clientconnectHandler sock chan numThreads server
  
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

clientconnectHandler :: Socket -> Chan String -> TVar Int -> DirectoryServer -> IO ()
clientconnectHandler sock chan numThreads server = do

  --Accept the socket which returns a handle, host and port
  --(handle, host, port) <- accept sock
  (s,a) <- accept sock
  --handle <- socketToHandle s ReadWriteMode
  --Read numThreads from memory and print it on server console
  count <- atomically $ readTVar numThreads
  putStrLn $ "numThreads = " ++ show count

  --If there are still threads remaining create new thread and increment (exception if thread is lost -> decrement), else tell user capacity has been reached
  if (count < maxnumThreads) then do
    forkFinally (clientHandler s chan server) (\_ -> atomically $ decrementTVar numThreads)
    atomically $ incrementTVar numThreads
    else do
      send s (pack ("Maximum number of threads in use. try again soon"++"\n\n"))
      sClose s

  clientconnectHandler sock chan numThreads server

clientHandler :: Socket -> Chan String -> DirectoryServer -> IO ()
clientHandler sock chan server@DirectoryServer{..} =
    forever $ do
        message <- recv sock 1024
	let msg = unpack message
        print $ msg ++ "!ENDLINE!"
        let cmd = head $ words $ head $ splitOn ":" msg
        print cmd
        case cmd of
            ("HELO") -> heloCommand sock server $ (words msg) !! 1
            ("KILL_SERVICE") -> killCommand chan sock
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
    
--Increment Tvar stored in memory i.e. numThreads
incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

--Decrement Tvar stored in memory i.e. numThreads
decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)
