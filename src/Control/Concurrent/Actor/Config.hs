{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Control.Concurrent.Actor.Config (
  startConfigSvc, startConfigSvcDefault,
  loadConfig) where

import BasicPrelude

import Data.Aeson (encode, object, (.=), Object, Value (Object, String))
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist, findFile)
import System.Environment (lookupEnv)

import Control.Concurrent.Actor (
    Behaviour (..), Channel, Message (..), MsgHandler, 
    defaultCtlHandler, defaultListener, dummyHandler, newChan, send, spawn)


type CKey = Text
type CValue = Text
type DSKey = Text
type DataSet = (DSKey, [(CKey, CValue)])

type ConfigStore = HM.HashMap DSKey (HashMap CKey CValue)


-- config actor

type ConfigRespChannel = Channel ConfigResponse

data ConfigRequest = ConfigQuery ConfigRespChannel DSKey
                   | ConfigUpdate DSKey CKey CValue

newtype ConfigResponse = ConfigResponse DataSet


startConfigSvcDefault :: IO ()
startConfigSvcDefault = 
    (lookupEnv "config-fco") >>= \case
      Just path -> startConfigSvc path
      _ -> startConfigSvc "../data/config-fco.yaml"

startConfigSvc :: FilePath -> IO ()
startConfigSvc path = do
    configData <- loadConfig path
    mb <- newChan
    spawn defaultListener [Behaviour mb configHandler] configData
    return ()

configHandler :: MsgHandler ConfigStore ConfigRequest
configHandler cfgData (Message (ConfigQuery rchannel key)) = do
    send rchannel $ Message (ConfigResponse (getDataFor key cfgData))
    return $ Just cfgData
configHandler cfgData (Message (ConfigUpdate dskey key value)) = 
    return $ Just $ updateData dskey key value cfgData
configHandler state msg = defaultCtlHandler state msg


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

