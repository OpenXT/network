--
-- Copyright (c) 2014 Citrix Systems, Inc.
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

module App ( App, AppState(..), NetworkObj, NetworkInfo (..), runApp, initAppState, getAppState, liftRpc, module Rpc) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import System.Process
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Data.IORef
import Data.IntSet (IntSet)
import Data.Map as M
import qualified Data.IntSet as IntSet
import Error
import Rpc

import Utils

data AppState = AppState {
                             wiredNetworks  :: MVar (M.Map NetworkObj NetworkInfo)
                             , wirelessNetworks  :: MVar (M.Map NetworkObj NetworkInfo)
                             , mobileNetworks  :: MVar (M.Map NetworkObj NetworkInfo)
                             , internalNetworks  :: MVar (M.Map NetworkObj NetworkInfo)
                             , slaveInitialized  :: MVar Bool
                             , newNetworkNames :: MVar Bool
                             , anyNetwork  :: MVar NetworkObj
                             , carrierDetectProcess :: MVar (Maybe ProcessHandle)
                         }

type NetworkObj = String

data NetworkInfo = NetworkInfo {
        nwIndex :: Int
        , nwInterface :: String
        , nwBridge :: String
    } deriving (Eq, Show)

newtype App a = App { unApp :: ReaderT AppState Rpc a }
    deriving (Functor, Monad, MonadIO, MonadError NwsError, MonadReader AppState)

initAppState :: IO AppState
initAppState = do
                  wiredNetworks <- newMVar M.empty
                  wirelessNetworks <- newMVar M.empty
                  mobileNetworks <- newMVar M.empty
                  internalNetworks <- newMVar M.empty
                  slaveInitialized <- newMVar False
                  newNetworkNames <- newMVar False
                  anyNetwork <- newMVar []
                  cdp <- newMVar Nothing
                  return (AppState wiredNetworks wirelessNetworks mobileNetworks
                          internalNetworks slaveInitialized newNetworkNames anyNetwork cdp)

runApp :: AppState -> App a -> Rpc a
runApp state app = runReaderT (unApp app) state

getAppState :: App AppState
getAppState = ask

liftRpc = App . lift
