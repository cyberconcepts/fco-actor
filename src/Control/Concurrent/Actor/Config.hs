{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Control.Concurrent.Actor.Config (
  spawnConfig, spawnConfigDef,
  loadConfig) where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist, findFile)
import System.Environment (lookupEnv)

import Control.Concurrent.Actor (
    Mailbox, Message (..), MsgHandler, 
    defCtlHandler, send, spawnDefActor)


type CKey = Text
type CValue = Text
type DSKey = Text
type DataSet = (DSKey, [(CKey, CValue)])

type ConfigStore = HM.HashMap DSKey (HashMap CKey CValue)


-- config actor

data ConfigRequest = ConfigQuery (Mailbox ConfigResponse) DSKey
                   | ConfigUpdate DSKey CKey CValue

newtype ConfigResponse = ConfigResponse DataSet


spawnConfigDef :: IO  (Mailbox ConfigRequest)
spawnConfigDef = 
    (lookupEnv "config-fco") >>= \case
      Just path -> spawnConfig path
      _ -> spawnConfig "../data/config-fco.yaml"

spawnConfig :: FilePath -> IO (Mailbox ConfigRequest)
spawnConfig path = loadConfig path >>= (spawnDefActor configHandler)

configHandler :: MsgHandler ConfigStore ConfigRequest
configHandler cfgData (Message (ConfigQuery respbox key)) = do
    send respbox $ Message (ConfigResponse (getDataFor key cfgData))
    return $ Just cfgData
configHandler cfgData (Message (ConfigUpdate dskey key value)) = 
    return $ Just $ updateData dskey key value cfgData
configHandler state msg = defCtlHandler state msg


-- storage handling

getDataFor :: DSKey -> ConfigStore -> DataSet
getDataFor dskey cfgData = 
    (dskey, HM.toList (HM.lookupDefault HM.empty dskey cfgData))

updateData :: DSKey -> CKey -> CValue -> ConfigStore -> ConfigStore
updateData dskey key value cfgData =
    HM.insert dskey (updateDS key value cfgData) cfgData
    where updateDS k v dat = 
            HM.insert k v (HM.lookupDefault HM.empty dskey dat)


loadConfig :: FilePath -> IO ConfigStore
loadConfig path = do
    let extractString = fmap (\(String vs) -> vs)
        extractObject f = fmap (\(Object vo) -> f vo)
    Right conf <- Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Object)
    return $ extractObject extractString conf

