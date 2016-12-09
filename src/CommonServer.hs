{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module CommonServer where

import           GHC.Generics
import           Data.Aeson

data File = File { fileName :: FilePath, fileContent :: String } deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

data CServer = CServer { address :: String, port :: String } deriving (Eq, Show, Generic)
instance ToJSON CServer
instance FromJSON CServer

data Response = Response { code :: Int, server :: CServer } deriving (Eq, Show, Generic)
instance ToJSON Response
instance FromJSON Response
