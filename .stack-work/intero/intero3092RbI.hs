{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommonServer where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
--import           Data.Bson.Generic
import           Servant

data File = File { fileName :: FilePath, fileContent :: String } deriving (Eq, Show, Generic)

instance ToJSON File
instance FromJSON File

data CServer = CServer { address :: String, port :: String } deriving (Eq, Show, Generic)
instance ToJSON CServer
instance FromJSON CServer

data Response = Response { code :: Int, server :: CServer } deriving (Generic, Show)
instance ToJSON Response
instance FromJSON Response
--instance ToBSON Response
--instance FromBSON Response

