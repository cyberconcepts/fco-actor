{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Actor where

import BasicPrelude

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    STM, TChan,
    atomically, newTChan, readTChan, tryReadTChan, writeTChan)
import Control.Monad.STM

import Control.Concurrent.Actor.Util (whileDataM)


-- actor, channel, message types

data Message a = Message a | QuitMsg 
  deriving (Eq, Ord, Show)

type Channel a = TChan (Message a)

type MsgHandler st a = st -> Message a -> IO (Maybe st)

data Behaviour st = forall a. Behaviour (Channel a) (MsgHandler st a)

type Actor st = [Behaviour st] -> st -> IO ()


-- actor functions

spawnActor :: Actor st -> [Behaviour st] -> st -> IO ThreadId
spawnActor listener behaviours state = 
    forkIO $ listener behaviours state

spawnDefActor :: MsgHandler st a -> st -> IO (Channel a)
spawnDefActor handler state = do
    mb <- newChan
    spawnActor defActor [Behaviour mb handler] state
    return mb

defActor :: Actor st
defActor behaviours = whileDataM $ \state -> receive state behaviours

dummyHandler :: MsgHandler st a
dummyHandler state (Message _) = return $ Just state
dummyHandler state msg = defCtlHandler state msg

defCtlHandler :: MsgHandler st a
defCtlHandler _ QuitMsg = return Nothing
defCtlHandler state _ = return $ Just state


-- messaging functions

newChan :: IO (Channel a)
newChan = atomically newTChan

send :: Channel a -> Message a -> IO ()
send chan msg = atomically $ writeTChan chan msg

receive :: st -> [Behaviour st] -> IO (Maybe st)
receive state behaviours =
    join (atomically $ processBehv state behaviours)
  where
      processBehv :: st -> [Behaviour st] -> STM (IO (Maybe st))
      processBehv _ [] = retry
      processBehv state (Behaviour chan handler : rest) =
        tryReadTChan chan >>= \case
            Nothing -> processBehv state rest
            Just msg -> return (handler state msg)

receiveChan :: Channel a -> IO (Message a)
receiveChan = atomically . readTChan

