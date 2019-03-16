{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright   :  (C) 2019 team@functionalconcepts.org
-- License     :  MIT
-- Maintainer  :  Helmut Merz <helmutm@cy55.de>
-- Stability   :  experimental
-- Portability :  GHC only (requires STM)
--
-- Provide configuration data via an 'Actor' that may be queried or updated.
--

module Control.Concurrent.Actor.Config (
-- * Types
  ConfigRequest (..), ConfigResponse (..), 
  ConfigStore, CKey, CValue, DSKey, DSValue, DataSet,
-- * Functions
  spawnConfig, spawnConfigDef, 
  loadConfig) where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist, findFile)
import System.Environment (lookupEnv)

import Control.Concurrent.Actor (
    Actor, Mailbox, MsgHandler, StdBoxes,
    send, spawnStdActor)


-- Configuration Store Types

-- | The key of a config dataset item.
type CKey = Text
-- | The value of a config dataset item.
type CValue = Text
-- | The key referencing a config dataset.
type DSKey = Text
-- | The dataset itself.
type DSValue = HashMap CKey CValue
-- | A dataset togegher with its key, as returned in a 'ConfigResponse' message.
type DataSet = (DSKey, DSValue)

-- | The configuration store, a two-level 'HashMap'.
type ConfigStore = HashMap DSKey DSValue


-- Request and Response Message Types

-- | A message sent to a config actor that either queries
-- the configuration for a certain key or updates a dataset.
data ConfigRequest = ConfigQuery DSKey (Mailbox ConfigResponse)
                   | ConfigUpdate DSKey CKey CValue

-- | The response sent back upon receiveing a 'ConfigQuery' request.
newtype ConfigResponse = ConfigResponse DataSet


-- Actor and Handler Functions

-- | Spawn a config actor by providing a default path to the 
-- yaml file with the configuration data.
--
-- The path is provided via the environment variable "config-fco";
-- if this is not set the default path "../data/config-fco.yaml" is used.
spawnConfigDef :: Actor st (StdBoxes ConfigRequest)
spawnConfigDef = 
    (liftIO $ lookupEnv "config-fco") >>= \case
      Just path -> spawnConfig path
      _ -> spawnConfig "../../data/config-fco.yaml"

-- | Load config data from the path given into the 'ConfigStore'
-- and spawn a config actor waiting for a 'ConfigRequest'.
spawnConfig :: FilePath -> Actor st (StdBoxes ConfigRequest)
spawnConfig path = do
    cfg <- liftIO $ loadConfig path
    spawnStdActor configHandler cfg

configHandler :: MsgHandler ConfigStore ConfigRequest
configHandler cfgData (ConfigQuery key respbox) = do
    send respbox $ ConfigResponse (getDataFor key cfgData)
    return $ Just cfgData
configHandler cfgData (ConfigUpdate dskey key value) = 
    return $ Just $ updateData dskey key value cfgData


-- Storage Handling

getDataFor :: DSKey -> ConfigStore -> DataSet
getDataFor dskey cfgData = 
    (dskey, HM.lookupDefault HM.empty dskey cfgData)

updateData :: DSKey -> CKey -> CValue -> ConfigStore -> ConfigStore
updateData dskey key value cfgData =
    HM.insert dskey (updateDS key value cfgData) cfgData
    where updateDS k v dat = 
            HM.insert k v (HM.lookupDefault HM.empty dskey dat)

-- | Load the 'ConfigStore' by reading the yaml file given.
loadConfig :: FilePath -> IO ConfigStore
loadConfig path = do
    let extractString = fmap (\(String vs) -> vs)
        extractObject f = fmap (\(Object vo) -> f vo)
    Right conf <- Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Object)
    return $ extractObject extractString conf

