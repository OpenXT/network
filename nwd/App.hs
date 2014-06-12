--
-- Copyright (c) 2012 Citrix Systems, Inc.
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module App ( App, AppState(..), runApp, initAppState, getAppState, liftRpc, module Rpc, permissibleSubnets, NetworkSlaveState(..), NetworkSlaveInfo(..), NwsObjInfo(..)) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Data.IORef
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Error
import Rpc
import qualified Data.Map as M

import NwTypes

-- Placeholder
-- TODO - Update the structure to maintain this information per network slave
-- (Currently, only a single slave is supported)
data AppState = AppState { subnets :: MVar IntSet
                         , bridgedNwObjs :: MVar (M.Map String NwsObjInfo)
                         , sharedNwObjs :: MVar (M.Map String NwsObjInfo)
                         , wirelessNwObjs :: MVar (M.Map String NwsObjInfo)
                         , internalNwObjs :: MVar (M.Map String NwsObjInfo)
                         , anyNwObjs :: MVar (M.Map String NwsObjInfo)
                         , nwsInfo :: MVar (M.Map Uuid NetworkSlaveInfo)
                         }

permissibleSubnets = IntSet.fromList [1..254]

data NwsObjInfo = NwsObjInfo {
     networkBackendUuid :: Uuid -- slave uuid
     , nwsObj :: String -- network slave obj
     , nwSubnet :: Int -- subnet id
} deriving (Eq,Show)
   
data NetworkSlaveInfo = NetworkSlaveInfo {
    networkBackendDomid :: DomainId
    , networkSlaveState :: NetworkSlaveState
    , networkSlaveObj :: String
} deriving (Eq,Show)

data NetworkSlaveState = Unknown | Registered | Initialized | Closed
                  deriving (Eq,Show)

newtype App a = App { unApp :: ReaderT AppState Rpc a }
    deriving (Functor, Monad, MonadIO, MonadError ApiError, MonadReader AppState)

initAppState :: IO AppState
initAppState = do subnets <- newMVar IntSet.empty
                  bridgedNwObjs <- newMVar M.empty
                  sharedNwObjs <- newMVar M.empty
                  wirelessNwObjs  <- newMVar M.empty
                  internalNwObjs <- newMVar M.empty
                  anyNwObjs <- newMVar M.empty
 
		  nwsInfo <- newMVar M.empty
                  return (AppState subnets bridgedNwObjs sharedNwObjs wirelessNwObjs internalNwObjs anyNwObjs nwsInfo)

runApp :: AppState -> App a -> Rpc a
runApp state app = runReaderT (unApp app) state

getAppState :: App AppState
getAppState = ask

liftRpc = App . lift
