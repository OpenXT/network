--
-- Copyright (c) 2013 Citrix Systems, Inc.
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

{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
module NetworkDomain where

import Control.Applicative
import System
import System.IO
import System.Posix.Syslog
import System.Posix.Process
import System.Posix.Signals
import System.FilePath.Posix ((</>))

import Rpc
import Rpc.Core
import App
import Error
import Tools.Log
import Tools.XenStore
import Tools.Process
import Tools.Misc
import Tools.Text

import Data.Int
import Data.Word
import Data.String
import Data.Bits
import Data.Maybe
import Data.List
import Data.IntSet (IntSet, (\\))
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import Text.Printf
import Text.Regex.Posix
import Directory

import Control.Monad.Trans
import Control.Monad.Error
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

import NwTypes
import NetworkSlaveMethods
import XSWifi
import NetworkDaemonConfig

import Rpc.Autogen.NetworkDomainServer
import qualified Rpc.Autogen.NetworkNmClient as NMC
import qualified Rpc.Autogen.NetworkSlaveClient as NWS
import Rpc.Autogen.NetworkDaemonNotify
import Rpc.Autogen.NetworkClient as NC

implementNetworkDomainInterface obj domid uuid = do
    debug ("Export - " ++ (show obj))
    appState <- getAppState
    transpBridging <- liftRpc $ configGetNdvmTransparentBridging uuid
               
    liftRpc $ rpcExpose (fromString obj) . interfaces $ NetworkDomainServer {
        comCitrixXenclientNetworkdomainListNetworks = runApp appState $ domainListNetworks uuid domid
        , comCitrixXenclientNetworkdomainConfigGetNmState = runApp appState $ domainNmState transpBridging domid
        , comCitrixXenclientNetworkdomainPopupNetworkMenu = \x y -> runApp appState $  nmPopupApplet domid x y
        , comCitrixXenclientNetworkdomainCloseNetworkMenu = runApp appState $ nmCloseApplet domid
	, comCitrixXenclientNetworkdomainConfigGetIsNetworkingActive  = runApp appState $ domainConnectivity transpBridging domid
        , comCitrixXenclientNetworkdomainConfigGetUuid = return $ uuidStr uuid
        , comCitrixXenclientNetworkdomainConfigGetDomid = return $ fromIntegral domid 
        , comCitrixXenclientNetworkdomainConfigGetName = runApp appState $ liftRpc $ getVmNameFromUuid  uuid
    }

matchNwsUuid uuid nwInfo = if (uuid == networkBackendUuid nwInfo)
			      then Left nwInfo
			      else Right nwInfo

matchNwsSlaveNw slaveObj nwInfo = if (slaveObj == nwsObj nwInfo)
			             then Left nwInfo
			             else Right nwInfo

getNetworksWithProperty matchFunc nwObjsMVar = do
    exportedNws <- liftIO $ readMVar nwObjsMVar
    let (matchedNws,_) = M.mapEither matchFunc exportedNws
    return matchedNws

matchBackendState state backendInfo = if (state == networkSlaveState backendInfo)
                                         then Left backendInfo
                                         else Right backendInfo

getBackendsWithProperty matchFunc backendsMVar = do
    knownBackends <- liftIO $ readMVar backendsMVar
    let (matchedBackends,_) = M.mapEither matchFunc knownBackends
    return matchedBackends
    

domainListNetworks :: Uuid -> DomainId -> App ([String])
domainListNetworks uuid domid = do 
    appState <- getAppState
    arrNws <- map M.keys <$> (mapM (getNetworksWithProperty (matchNwsUuid uuid)) $ nwMvarList appState)
    return $ concat arrNws

nmPopupApplet :: DomainId -> Word32 -> Word32 -> App ()
nmPopupApplet domid x y = liftRpc $ 
    withNetworkSlave domid (NMC.comCitrixXenclientNetworkNmPopupNetworkMenu slaveService "/nm" x y)

nmCloseApplet :: DomainId -> App ()
nmCloseApplet domid = liftRpc $ 
    withNetworkSlave domid (NMC.comCitrixXenclientNetworkNmCloseNetworkMenu slaveService "/nm")

-- map slave network obj to daemon (exposed) obj
exposedNwForSlaveNw appState uuid slaveObj = do
    slaveNws <- mapM (getNetworksWithProperty (matchNwsUuid uuid)) $ nwMvarList appState --networks provided by that backend
    let (nwMap,_) = M.mapEither (matchNwsSlaveNw slaveObj) $ M.unions slaveNws

    if (M.size nwMap == 1)
       then return nwMap
       else do debug $ printf "Can't find the exact match for network %s (%s) - matched networks - %s" slaveObj (uuidStr uuid) (show nwMap)
               return M.empty

domainConnectivity transpBridging domid = liftRpc $
    if (transpBridging == True)
       then (return True)
       else withNetworkSlave domid (NWS.comCitrixXenclientNetworkslaveNwConnectivity slaveService slaveRootObj)

domainNmState transpBridging domid = do
    if (transpBridging == True)
       then (return 100) --Hack to make nm-applet show up as connected in transparent bridging state
       else liftRpc $ withNetworkSlave domid (NWS.comCitrixXenclientNetworkslaveNmState  slaveService slaveRootObj)

networkStateChanged :: AppState -> MVar (Maybe ScheduledTask) -> DBusId -> DomainId -> Uuid -> ObjectPath -> [Variant] -> Rpc ()
networkStateChanged appState task_mv dbusid domid uuid slaveNw args = do
    if null args
       then warn "Received state_changed signal with no object path."
       else do 
          knownSlaves <- liftIO $ readMVar (nwsInfo appState)
          initialized <- checkNetworkDomainState Initialized knownSlaves uuid domid
          if (initialized == True)
             then do
               nwMap <- exposedNwForSlaveNw appState uuid (objPathToStr slaveNw)
               case (M.toList nwMap) of 
                    [(nwObj, nwInfo)] -> do mapM_ (emitStateChanged nmState (networkBackendObj uuid)) $ nub $ nwObj:(bridgedPair nwObj):[]
                                            scheduleSynchronise task_mv appState domid uuid 
                    otherwise -> debug $ printf "Failed to emit network_state_changed signal - %s" (show (M.toList nwMap))
             else warn $ printf "Received state_changed signal from a slave that's not initialized (%s)" (show uuid)

    where (nmState :: Word32) = varToStr args
          bridgedPair = replace eCONNECTION_TYPE_BRIDGED eCONNECTION_TYPE_SHARED 
          emitStateChanged nmState backend nwObj = notifyComCitrixXenclientNetworkdaemonNotifyNetworkStateChanged (fromString nwdRootObj) nwObj (show nmState) backend

networkBackendObj uuid = "/ndvm/" ++  (uuidStrUnderscore uuid)

domainWithVif :: String -> App ([NetworkSlaveInfo])
domainWithVif vif = do
    initializedBackends <- getAppState >>= \appState -> getBackendsWithProperty (matchBackendState Initialized) (nwsInfo appState)
    matchedBackend <- catMaybes <$>  mapM (domHasVif vif) (M.elems initializedBackends)
    debug $ printf "domainWithVif %s %s" vif (show matchedBackend)
    return matchedBackend

    where
        domHasVif :: String -> NetworkSlaveInfo -> App (Maybe NetworkSlaveInfo)
        domHasVif vif backendInfo = do
                 let domid = networkBackendDomid backendInfo
                 vifs <- liftRpc $ withNetworkSlave domid (NWS.comCitrixXenclientNetworkslaveListVifs slaveService slaveRootObj)
                 if (elem vif vifs)
                    then return $ Just backendInfo
                    else return Nothing

scheduleSynchronise task_mv appState domid uuid = do
     rpc_ctx <- rpcGetContext
     liftIO $ modifyMVar_ task_mv $ f rpc_ctx
        where f ctx Nothing  = Just <$> schedule (fromIntegral 2) (sync_action ctx)
              f ctx (Just t) = reschedule (fromIntegral 2) t >> return (Just t)
              -- reset task mvar to Nothing after sync completes
              sync_action ctx = modifyMVar_ task_mv (\_ -> (handleErrors =<< rpc ctx (syncAllNdvmVifs appState domid uuid)) >> return Nothing)
              handleErrors (Left x) = debug $ printf "scheduleSynchronise error - %s" (show x)
              handleErrors _        = return ()

-- Update the xenstore nodes for the vifs using this backend network
syncAllNdvmVifs :: AppState -> DomainId -> Uuid -> Rpc ()
syncAllNdvmVifs appState domid uuid = void $ do
   guestDomids <- liftIO $ xsGuestDomains domidS
   networks <- M.unions <$> (mapM (getNetworksWithProperty (matchNwsUuid uuid)) $ nwMvarList appState) 
   mapM (updateGuestNetworkState networks) guestDomids

   where 
        domidS = show domid
        updateGuestNetworkState networks guestDomid = do
            devs <- liftIO $ xsDevIds domidS guestDomid
            mapM_ (updateVifNetworkState networks guestDomid) devs

        updateVifNetworkState networks guestDomid guestDevid = do
            nw <- vmNetworkConfig guestDomid guestDevid domidS
            case nw of
                 Just v -> case (M.lookup v networks) of
                                Just nw -> void $ do 
                                     syncVifState domid (read guestDomid) guestDevid nw
                                otherwise ->  return ()
                 Nothing -> return ()

syncVifState :: DomainId -> DomainId -> String -> NwsObjInfo -> Rpc ()
syncVifState slaveDomid guestDomid guestDevid nw = do
    let slaveNw = nwsObj  nw
    updateWifiXS slaveDomid slaveNw guestDomid
    carrier <- withNetworkSlave slaveDomid (NC.comCitrixXenclientNetworkConfigGetActive slaveService slaveNw)
    debug $ printf "syncAllNdvmVifs xsSetVifDisconnect %s %s %s %s" guestDomidS guestDevid slaveDomidS (show carrier)
    liftIO $ xsSetVifDisconnect guestDomidS guestDevid slaveDomidS (disconnect carrier)

    where
        slaveDomidS = show slaveDomid
        guestDomidS = show guestDomid
        disconnect True = "0"
        disconnect False = "1"

updateWifiXS :: DomainId -> String -> DomainId -> Rpc () 
updateWifiXS domid slaveNw guestDomid = do
    debug $ printf "updateWifiXS  %s %s %s" (show domid) slaveNw (show guestDomid)
    nwType <- withNetworkSlave domid (NC.comCitrixXenclientNetworkConfigGetType slaveService slaveNw)
    case () of
        _ | (nwType == eNETWORK_TYPE_WIFI) -> wifiXsQueryAndExport domid slaveNw guestDomid
          | otherwise -> return ()
