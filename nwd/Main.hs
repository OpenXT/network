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

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System
import System.IO
import System.Posix.Syslog
import System.Posix.Process
import System.Posix.Signals
import System.FilePath.Posix ((</>))
import System.Directory

import Rpc
import Rpc.Core
import App
import Error
import Tools.Log
import Tools.XenStore
import Tools.Process
import Tools.File
import Tools.Text
import Tools.Misc

import Data.Int
import Data.Char
import Data.Word
import Data.String
import Data.Maybe
import Data.List (nub)
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
import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import NwTypes
import NetworkSlaveMethods
import NetworkDomain
import NetworkDaemonConfig

import Rpc.Autogen.NetworkDaemonServer as NWD
import Rpc.Autogen.NetworkServer as NS
import Rpc.Autogen.NetworkDaemonNotify
import Rpc.Autogen.NetworkDomainNotify
import qualified Rpc.Autogen.NetworkSlaveClient as NWS
import qualified Rpc.Autogen.NetworkClient as NC
import Rpc.Autogen.NetworkDaemonConst
import Rpc.Autogen.NetworkConst

-- Main
main :: IO ()
main = void $ do 
          appState <- initAppState
          doService appState nwdService $ do 
                implementNWDServer

-- Start the service and the implementations
-- TODO - Error Handling
doService appState service implementation = rpcServe service $ \rpcContext -> do
        rpc rpcContext $ runApp appState $ implementation
        rpc rpcContext $ exportNetworkDaemonConfig 
        
        let certsPath = configPath  </> "certs"
        liftIO $ createDirectoryIfMissing False  certsPath
        debug $ printf "Wait for xenmgr to notify about the NDVM"
        
        -- live forever
        (liftIO . forever $ threadDelay (10^6 * 60))

attachSignalHandlers appState uuid domid = do
    debug $ printf "Register for network slave signals in domain:%d" domid
    onNetworkSlaveSignal domid "networkslave_up" $ newSlave appState
    onNetworkSlaveSignal domid "new_backend_vif" (newBackendVif appState)
    onNetworkSlaveSignal domid "network_added" (networkAdded appState)
    onNetworkSlaveSignal domid "network_removed" (networkRemoved appState)

    sync_task <- liftIO $ newMVar Nothing
    onNetworkSignal domid "state_changed" (networkStateChanged appState sync_task)

waitForNetworkSlave :: AppState -> DomainId -> Uuid -> App ()
waitForNetworkSlave appState domid uuid = do
    knownSlaves <- appStateSlaveInfo
    nwDomainRegistered <- isKnownNetworkDomain knownSlaves uuid domid
    if nwDomainRegistered
       then debug $ printf "Already listening to the slave in domain:%d" domid
       else do 
            liftRpc $ attachSignalHandlers appState uuid domid
            saveNwsInfo
            debug $ printf "Emitting networkdaemon up and listening to signals from networkslave in Dom%d" domid
            liftRpc $ do withNetworkSlave domid (notifyComCitrixXenclientNetworkdaemonNotifyNetworkdaemonUp nwdRootObjPath)

    return ()

    where
          saveNwsInfo = do
            let slaveInfo  = M.singleton uuid  NetworkSlaveInfo{networkBackendDomid=domid, networkSlaveState=Registered, networkSlaveObj=ndvmObj}
            knownSlaveInfo <- liftIO $ takeMVar nwsInfoMVar
            liftIO $ putMVar nwsInfoMVar (slaveInfo `M.union` knownSlaveInfo)
            implementNetworkDomainInterface ndvmObj domid uuid
            where nwsInfoMVar = nwsInfo appState

          ndvmObj = networkBackendObj uuid
            
-- DBus signal handler
--onNetworkSlaveSignal :: DomainId -> String ->  -> Rpc ()
onNetworkSlaveSignal domid msgname action = 
    let rule = matchSignal "com.citrix.xenclient.networkslave.notify" msgname
    in
      nwsOnSignal domid rule action

onNetworkSignal domid msgname action = 
    let rule = matchSignal "com.citrix.xenclient.network.notify" msgname
    in
      nwsOnSignal domid rule action


nwdRootObjPath = fromString nwdRootObj

configureVirtualInterface appState domid devid mac = liftIO $ void $ do
    debug $ printf "Configure virtual interface on dom0 with mac : %s" mac
    mapM_ (xsVifRemove "0") ["0", "1"] 
    system $ xenopsAddVifCmd "0" (show domid) mac devid

configureDom0IP = do
    -- assuming that at any given time, dom0 in XT will have only one active interface
    (exitCode, _,_) <- readProcessWithExitCode_closeFds "ifconfig" ["eth0"] []
    case exitCode of
	 ExitSuccess -> do debug "Configure IP address for dom0 interface"
                           pid <- strip <$> fromMaybe "" <$> maybeGetContents udhcpcPidFile
                           unless (null pid) $ void $ system $ "kill " ++ pid 
			   spawnShell $ printf "udhcpc -b -i eth0 -p %s" udhcpcPidFile
	 otherwise -> do threadDelay 1000
			 configureDom0IP
    where udhcpcPidFile = "/var/run/udhcpc.pid"
  
-- Handler for networkslave_up signal
-- This is the first signal from the slave. Configure all the networks
-- exported by the slave and  map the networks as network daemon networks
-- for other processes to access.
newSlave :: AppState -> DBusId -> DomainId -> Uuid -> ObjectPath -> [Variant] -> Rpc ()
newSlave appState dbusid domid uuid path args = void $ do 
    knownSlaves <- liftIO $ readMVar (nwsInfo appState)
    nwDomainRegistered <- isKnownNetworkDomain knownSlaves uuid domid
    if nwDomainRegistered
       then do info $ "Received networkslave_up signal from uuid: " ++ show uuid ++ " domid: " ++ show domid ++ " args: " ++ show args
               initializeNetworkSlave appState dbusid domid uuid args
       else do debug $ printf "Received networkslave signal before registering the slave. So ignoring networkslave_up from %d" domid

updateNetworkSlaveDomstore domid uuid = do
    filteringConf <- configGetNdvmDomstore uuid NWS.eCONFIG_BRIDGE_FILTERING
    -- unless specified, the default option is to enable bridge filtering in NDVM
    when (null filteringConf) $ configSetNdvmDomstore uuid NWS.eCONFIG_BRIDGE_FILTERING True

    debug $ printf "Bridge filtering configuration for %s : %s" (uuidStr uuid) (show filteringConf)
    transpBridging <- configGetNdvmTransparentBridging uuid
    debug $ printf "Transparent bridging configuration for %s : %s" (uuidStr uuid) (show transpBridging)
    if (transpBridging == True)
       then configSetNdvmDomstoreS uuid NWS.eCONFIG_NM_UNMANAGED_DEVICES "all"
       else configSetNdvmDomstoreS uuid NWS.eCONFIG_NM_UNMANAGED_DEVICES ""
            -- TODO when set unmanaged property is enabled for network objects, this needs to be updated accordingly

initializeNetworkSlave appState dbusid domid uuid args = do
    updateNetworkSlaveDomstore domid uuid
    nws <- initNetworkSlave appState dbusid domid uuid
    liftIO $ updateNwsState appState uuid Initialized
    notifyComCitrixXenclientNetworkdomainNotifyBackendStateChanged backendObj eNDVM_STATUS_STARTED

    addSlaveBackendVifsToNetworks domid

    dom0Network  <- fromMaybe "" <$> getDom0Network
    when (dom0Network `elem` nws) $ setupDom0Networking appState dom0Network

    where backendObj = fromString $ networkBackendObj uuid

updateNwsState appState uuid state = do
    modifyMVar_ (nwsInfo appState) $ 
 	return . M.adjust (\slaveInfo -> NetworkSlaveInfo{networkBackendDomid=networkBackendDomid slaveInfo, networkSlaveState=state, networkSlaveObj=networkSlaveObj slaveInfo}) uuid

addSlaveBackendVifsToNetworks domid = withNetworkSlave domid (NWS.comCitrixXenclientNetworkslaveRefreshVifs slaveService slaveRootObj)

addVifToNetwork :: DomainId -> Uuid -> DomainId -> String -> String -> Rpc ()
addVifToNetwork slaveDomid slaveUuid guestDomid vif nwObj = do
    debug $ printf "addVifToNetwork - %d %s %d %s %s" slaveDomid (show slaveUuid) guestDomid vif nwObj
    if (guestDomid == 0)
       then do
            -- TODO read dom0 firewall config from somewhere.For now allow all traffic from dom0
            return ()
       else do
            guestUuidM <- getDomainUuid guestDomid
            guestStubdomidM <- liftIO $ domWithStubdom guestDomid
            gVmObj <- case (guestUuidM == Nothing, guestStubdomidM == Nothing) of
                           (False, False) -> getVmFromDomid (read (fromJust guestStubdomidM) :: Int32)
                           (False, True) -> getVmFromDomid (fromIntegral guestDomid)
                           otherwise -> return ""
            unless (null gVmObj) $ do 
                filters <- getVmFirewallRules gVmObj
                debug $ printf "VM firewall rules - %s %s" gVmObj (show filters)
                mapM_ (setFirewallRules slaveUuid (fromJust guestUuidM)) filters
    withNetworkSlave slaveDomid (NWS.comCitrixXenclientNetworkslaveMoveVifToNetwork slaveService slaveRootObj vif nwObj)

   where
        setFirewallRules :: Uuid -> Uuid -> (M.Map String String) -> Rpc ()
        setFirewallRules slaveUuid guestUuid filter = configSetFirewallRules slaveUuid guestUuid id direction ip extra
                                                      where direction = fromJust $ M.lookup dsFirewallDirection filter 
                                                            id = fromJust $ M.lookup dsFirewallId filter
                                                            extra = fromJust $ M.lookup dsFirewallExtra filter
                                                            ip = fromJust $ M.lookup dsFirewallRemoteIp filter

-- Handler for new backend vif signal from the slave. Retrieve the network information and join the vif to that network
newBackendVif :: AppState -> DBusId -> DomainId -> Uuid -> ObjectPath -> [Variant] -> Rpc ()
newBackendVif appState dbusid domid uuid path args = do
   if null args
        then warn "Received new_backend_vif signal without vif information."
        else do
             knownSlaves <- liftIO $ readMVar (nwsInfo appState)
             initialized <- checkNetworkDomainState Initialized knownSlaves uuid domid
             case (initialized, (varToArrStr args)) of
                  (True, [vif, guestDomid, devid]) -> do
                     liftIO $ xsSetVifDisconnect guestDomid devid (show domid) "1" 
	             maybeNwInfo <- (vmNetworkConfig guestDomid devid (show domid)) >>= (liftIO . getSlaveInfo appState)
		     case maybeNwInfo of
		          Just nw -> do 
                            debug $ printf "Move %s to %s network" vif (show $ nw)
			    --withNetworkSlave domid (NWS.comCitrixXenclientNetworkslaveMoveVifToNetwork slaveService slaveRootObj vif (nwsObj nw))
                            addVifToNetwork domid uuid (read guestDomid :: Int) vif (nwsObj nw)
                            when ((read guestDomid :: Int32) == 0) $ void $ liftIO $ configureDom0IP 
                            syncVifState domid (read guestDomid) devid nw

             	          Nothing -> return ()
                  (False, _) -> debug $ printf "Received new_backend_vif signal from a slave that's not initialized (%s)" (show uuid)
                  otherwise -> debug $ printf "Ignoring new_backend_vif signal - invalid args: %s" (show args)

-- Handler for network_added signal
networkAdded :: AppState -> DBusId -> DomainId -> Uuid -> ObjectPath -> [Variant] -> Rpc ()
networkAdded appState dbusid domid uuid path args = do
    if null args
        then warn "Received network_added signal with no object path."
        else do 
            knownSlaves <- liftIO $ readMVar (nwsInfo appState)
            initialized <- checkNetworkDomainState Initialized knownSlaves uuid domid
            if (initialized == True)
               then do  
                    mapM_ (handleNetworkAdded appState uuid domid) networkObjs
                    addSlaveBackendVifsToNetworks domid
               else warn $ printf "Received network_added signal from a slave that's not initialized (%s)." (show uuid)
    where 
        networkObjs = varToArrStr args
     
        withNws = withNetworkSlave domid
 
        handleNetworkAdded appState uuid domid networkObj = do
            debug  $ printf "Handle network_added notification from nws (dom%d) for %s" domid networkObj
            nwdObj <- configureSlaveNetwork appState uuid domid networkObj
            unless (null nwdObj) $ do
                  notifyComCitrixXenclientNetworkdaemonNotifyNetworkAdded nwdRootObjPath nwdObj

                  dom0Network  <- fromMaybe "" <$> getDom0Network
                  when (dom0Network == nwdObj) $ setupDom0Networking appState dom0Network

-- Handler for network_removed signal
networkRemoved :: AppState -> DBusId -> DomainId-> Uuid -> ObjectPath -> [Variant] -> Rpc ()
networkRemoved appState dbusid domid uuid path args = do
    if null args
        then warn "Received network_removed signal with no object path."
        else do
            knownSlaves <- liftIO $ readMVar (nwsInfo appState)
            initialized <- checkNetworkDomainState Initialized knownSlaves uuid domid
            if (initialized == True)
               then mapM_ (handleNetworkRemoved appState uuid) networkObjs
               else warn $ printf "Received network_removed from a slave that's not initialized (%s)." (show uuid)

    where 
        networkObjs = varToArrStr args

        handleNetworkRemoved appState uuid networkObj = do 
            nwMap <- exposedNwForSlaveNw appState uuid networkObj
            case (M.toList nwMap) of 
                 [(nwdObj, nwInfo)] -> do hideObj nwdObj
                                          --notifyComCitrixXenclientNetworkdaemonNotifyNetworkRemoved (fromString nwdRootObj) nwdObj 
                                          exportNetworkWithName nwdObj
                 other -> return ()

-- Initialize network slave
initNetworkSlave :: AppState -> DBusId -> DomainId -> Uuid -> Rpc [String]
initNetworkSlave appState dbusid domid uuid = do
    debug $ printf "initNetworkSlave - signal from uuid: %s domid: %s" (uuidStr uuid) (show domid)

    cleanupSlaveObjs appState uuid domid
    configureSlave appState dbusid domid uuid

getDom0Network ::  Rpc (Maybe String)
getDom0Network = do
    dom0Networking <- configDom0NetworkingEnabled
    dom0Nw  <- configDom0Network
    return $ if dom0Networking
                then (Just dom0Nw)
                else Nothing

getNetworkBackendInfo :: AppState -> String -> Rpc (Maybe Uuid, Maybe DomainId, Maybe String)
getNetworkBackendInfo appState network = do
    maybeNwInfo <- liftIO $ getSlaveInfo appState (Just network)
    case maybeNwInfo of
         Just nwInfo -> do 
             backendInfo <- liftIO $ readMVar (nwsInfo appState)
             let uuid = networkBackendUuid nwInfo
             let slaveNw = nwsObj nwInfo
             case (M.lookup uuid backendInfo) of
                  Just info -> let domid = networkBackendDomid info
                               in return (Just uuid, Just domid, Just slaveNw)
                  otherwise -> return (Nothing, Nothing, Nothing)
         otherwise -> return (Nothing, Nothing, Nothing) 

setupDom0Networking appState network = do
    (getNetworkBackendInfo appState network) >>= f
    where f (Nothing, Nothing, Nothing) = return ()
          f (Just uuid, Just domid, Just nw) = configureDom0Networking appState uuid domid network nw 

configureDom0Networking appState uuid domid network slaveNw = void $ do
    nwType <- withNws (NC.comCitrixXenclientNetworkConfigGetType slaveService slaveNw)
    mac <- withNws (NC.comCitrixXenclientNetworkConfigGetMacAddress slaveService slaveNw)
    if (nwType == eNETWORK_TYPE_WIFI  || nwType == eNETWORK_TYPE_MODEM)
       then do
            configureVirtualInterface appState domid "1" (virtualMac mac)
       else if (nwType == eNETWORK_TYPE_WIRED) 
            then configureVirtualInterface appState domid "0" (realMac mac)
            else configureVirtualInterface appState domid "0" (realMac mac) -- TODO generate virtual mac before adding dom0 vif to brany or brinternal network
   where
        withNws = withNetworkSlave domid

-- Configure networks exported by the slave, start networ manager and
-- refresh vifs (move them to their configured networks)
configureSlave :: AppState -> DBusId -> DomainId -> Uuid -> Rpc [String]
configureSlave appState dbusid domid uuid = do
    debug $ printf "configureSlave - dbusid %s - dom%d - uuid %s" dbusid domid (uuidStr uuid)
    allInternalNws <- configGetNetworksOfType eNETWORK_TYPE_INTERNAL
    let internalNwsCurrentDom = M.filter (\network -> (getConfigProperty network eNW_PROP_UUID) == Just (uuidStr uuid)) allInternalNws
    let internalNwsNumber = if (M.null internalNwsCurrentDom)
                                  then 1
                                  else M.size internalNwsCurrentDom
    withNws (NWS.comCitrixXenclientNetworkslaveCreateInternalNetworks slaveService slaveRootObj (fromIntegral internalNwsNumber))
    nwObjs <- withNws (NWS.comCitrixXenclientNetworkslaveListNetworks slaveService slaveRootObj)

    nws <- mapM (configureSlaveNetwork appState uuid domid) nwObjs

    withNws (NWS.comCitrixXenclientNetworkslaveStartNm slaveService slaveRootObj)
    return nws

    where withNws = withNetworkSlave domid

getUsedIndices :: (M.Map String NwsObjInfo) -> [Int]
getUsedIndices existingNws = map match (M.keys existingNws)
    where match str = let (_,index,_,_) = str=~ "[0-9]+" :: (String, String, String, [String])
                      in read index

configureNetwork :: AppState -> DomainId -> String -> String -> String -> String -> Rpc ()
configureNetwork appState domid nwObj natPrefix nwMac nwdNetwork = do
    isConfigured <- withNws (NC.comCitrixXenclientNetworkIsConfigured slaveService nwObj)
    connType <- withNws (NC.comCitrixXenclientNetworkConfigGetConnection slaveService nwObj)
    nwType <- withNws (NC.comCitrixXenclientNetworkConfigGetType slaveService nwObj)

    unless (isConfigured) $ void $ do
           case () of
                _ | (connType == eCONNECTION_TYPE_SHARED && nwType /= eNETWORK_TYPE_INTERNAL) -> do
                        freeId <- getSharedNwConfig appState natPrefix
                        if (null freeId)
                            then fatal "No empty subnets available."
                            else withNws (NC.comCitrixXenclientNetworkConfigure slaveService nwObj freeId)
                  | (connType == eCONNECTION_TYPE_BRIDGED && nwType == eNETWORK_TYPE_WIRED) -> do
                        dom0Network <- fromMaybe "" <$> getDom0Network
                        if (dom0Network == nwdNetwork)
                           then withNws (NC.comCitrixXenclientNetworkConfigure slaveService nwObj (virtualMac nwMac))
                           else withNws (NC.comCitrixXenclientNetworkConfigure slaveService nwObj "")
                  | otherwise -> withNws (NC.comCitrixXenclientNetworkConfigure slaveService nwObj "")

    where 
          withNws = withNetworkSlave domid
          subnetsMVar = subnets appState

-- Configure a slave network and exportObj it
configureSlaveNetwork :: AppState -> Uuid -> DomainId -> String -> Rpc (String)
configureSlaveNetwork appState uuid domid nwObj = do
    info $ "Configuring network" ++ show nwObj
    
    connType <- withNws (NC.comCitrixXenclientNetworkConfigGetConnection slaveService nwObj)
    nwType <- withNws (NC.comCitrixXenclientNetworkConfigGetType slaveService nwObj)

    mac <- withNws (NC.comCitrixXenclientNetworkConfigGetMacAddress slaveService nwObj)
  
    nw <- case () of
           _  | (nwType == eNETWORK_TYPE_ANY) -> implementNwdObj (anyNwObjs appState)  nwType connType ""
              | (nwType == eNETWORK_TYPE_WIFI || nwType == eNETWORK_TYPE_MODEM) -> implementNwdObj (wirelessNwObjs appState) nwType connType mac
              | (nwType == eNETWORK_TYPE_INTERNAL) -> implementNwdObj (internalNwObjs appState) nwType connType ""
              | (nwType == eNETWORK_TYPE_WIRED) -> 
                           if (connType == eCONNECTION_TYPE_SHARED)
                              then implementNwdObj (sharedNwObjs appState) nwType connType mac
                              else implementNwdObj (bridgedNwObjs appState) nwType connType mac
              | otherwise -> return ""

    return nw

    where 
        withNws = withNetworkSlave domid

        implementNwdObj nwObjsMVar nwType nwSubType vMac = do
            let rMac = realMac vMac
            existingNws <- liftIO $ takeMVar nwObjsMVar
            matchingConfigs <- getMatchingNetworkConfigs nwType (uuidStr uuid) rMac
            let unusedConfigs = filterConfigs matchingConfigs existingNws nwType nwSubType
            freeIndex <- getFreeIndex existingNws nwType
            let (index, configStr) = case unusedConfigs of
                                            [] ->  (freeIndex, "")
                                            otherwise -> (head unusedConfigs)
            let nwName = dbusObjFromNwType index nwType nwSubType
                nwInfo = M.singleton nwName NwsObjInfo{networkBackendUuid=uuid, nwsObj=nwObj, nwSubnet=0}
                label = fromMaybe "" $ getConfigProperty configStr eNW_PROP_LABEL
                prefix = fromMaybe "" $ getConfigProperty configStr eNW_PROP_NATPREFIX

            configureNetwork appState domid nwObj prefix rMac nwName
            hideObj nwName
 
            debug $ printf "Update database with network %s" nwName 
            configSetNetwork  nwType index $ M.fromList [(eNW_PROP_UUID, uuidStr uuid)
                                                         , (eNW_PROP_MAC, rMac)
                                                         , (eNW_PROP_LABEL, label)
                                                         , (eNW_PROP_NATPREFIX, prefix)]
          
            transpBridging <- configGetNdvmTransparentBridging uuid
            implementNetwork uuid domid nwObj nwName transpBridging
            liftIO $ putMVar nwObjsMVar (nwInfo `M.union` existingNws)
            return nwName
 
        getFreeIndex :: M.Map String NwsObjInfo -> String -> Rpc String
        getFreeIndex existingNws nwType = do
                fixedIds <- map (read :: String -> Int)  <$> configNetworksList (nwType)
                return $ show $ IntSet.findMin ((allIds \\ usedIds) \\ (IntSet.fromList fixedIds))
	    where
		allIds = IntSet.fromList[0..253]
		usedIds = IntSet.fromList $ getUsedIndices existingNws 
        -- filter out configs used to configure any existing network
        filterConfigs configs nws nwType nwSubType = filter (\(idx, _) -> isNothing (M.lookup (dbusObjFromNwType idx nwType nwSubType) nws)) configs
    
-- TODO - Make this better
getSharedNwConfig :: AppState -> String -> Rpc String
getSharedNwConfig appState natPrefix = do
    if null natPrefix
       then assignNatConf
       else case (natPrefix `matchG` "([0-9]+\\.[0-9]+)\\.([0-9]+)\\.[0-9]+") of -- TODO check valid ip range
                [prefix, id] -> return (nwConfStr prefix id) -- TODO handle if this is of 172.16 prefix
                otherwise -> assignNatConf

    where
        assignNatConf = do 
            usedSubnets <- liftIO $ takeMVar subnetsMVar
            let freeSubnets = permissibleSubnets \\ usedSubnets
            if IntSet.null freeSubnets
               then do liftIO $ putMVar subnetsMVar usedSubnets
                       return ""
               else let freeIdx = IntSet.findMin freeSubnets in
                    do liftIO $ putMVar subnetsMVar (IntSet.insert freeIdx usedSubnets)
                       return (nwConfStr "172.16" (show freeIdx))

        subnetsMVar = subnets appState

        nwConfStr :: String -> String -> String
        nwConfStr prefix id = let dhcpStart :: String = printf "%s.%s.10" prefix id
                                  dhcpEnd :: String = printf "%s.%s.100" prefix id
                                  bridgeIP :: String = printf "%s.%s.1" prefix id
                                  netmask = "255.255.255.0"
                              in printf "%s %s %s %s" dhcpStart dhcpEnd bridgeIP netmask

-- Hide all the exported networks and update the state
cleanupSlaveObjs :: AppState -> Uuid -> DomainId -> Rpc ()
cleanupSlaveObjs appState uuid domid = void $ do
    mapM_ cleanupSlaveNetworks $ nwMvarList appState

    where
        subnetsMVar = subnets appState
        nwsInfoMVar = nwsInfo appState 

        cleanupSlaveNetworks nwObjsMVar = void $ do
            exportedNws <- liftIO $ readMVar nwObjsMVar
            let (matchedNws, unmatchedNws) = M.mapEither (matchNwsUuid uuid) exportedNws

            unless (M.null matchedNws) $ do
                info ("Remove all the old exported networks " ++ (show $ M.keys matchedNws))
                mapM_ (cleanupState appState) $ M.toList matchedNws

                info "Reset the appState nwObjs"
                liftIO $ swapMVar nwObjsMVar (unmatchedNws)

                -- Remove vifs on Dom0 if Dom0 network is removed
                dom0Network  <- fromMaybe "" <$> getDom0Network
		when (elem dom0Network $ M.keys matchedNws) $ void $ 
                      liftIO $ do system $ xenopsDelVifCmd "0" (show domid) "0" 
                                  system $ xenopsDelVifCmd "0" (show domid) "1"
 
        cleanupState appState (nwObj, nwInfo) = do
            -- freeSubnets <- liftIO $ takeMVar subnetsMVar
            -- liftIO $ putMVar subnetsMVar (IntSet.delete (nwSubnet nwInfo) freeSubnets)
            hideObj nwObj
            exportNetworkWithName nwObj 

{-----------------------------------------------------------------------------
Network daemon interfaces
------------------------------------------------------------------------------}
-- Handle interfaces
implementNWDServer :: App ()
implementNWDServer = do
  appState <- getAppState
  liftRpc $ rpcExpose (fromString nwdRootObj) . NWD.interfaces $ NetworkDaemonServer
    { comCitrixXenclientNetworkdaemonAddVif = \_ _ _ -> return () -- TODO - XT support
    , comCitrixXenclientNetworkdaemonMoveToNetwork  = \vif nw -> runApp appState $ moveToNetwork vif nw
    , comCitrixXenclientNetworkdaemonShutdown = return ()
    , comCitrixXenclientNetworkdaemonIsNetworkingActive = runApp appState $ isNetworkingActive
    , comCitrixXenclientNetworkdaemonList = runApp appState $ listNetworks
    , comCitrixXenclientNetworkdaemonListBackends = runApp appState $ listNetworkBackends
    , comCitrixXenclientNetworkdaemonIsInitialized = return True 
    , comCitrixXenclientNetworkdaemonNdvmStatus = \uuid domid status -> runApp appState $ ndvmStatusUpdate uuid domid status
    , comCitrixXenclientNetworkdaemonCreateNetwork = createNetwork
    , comCitrixXenclientNetworkdaemonGetNetworkBackend = \nw -> runApp appState $ getNetworkBackend nw
    }

ndvmStatusUpdate :: String -> Int32 -> Word32 -> App ()
ndvmStatusUpdate uuid domid status = do
    debug $ printf "NDVM status update from xenmgr: %s %d %u" uuid domid status
    appState <- getAppState
    
    case () of
      _ | status == eNDVM_STATUS_STARTED -> do
                        waitForNetworkSlave appState domidI uuidU 
        | status == eNDVM_STATUS_STOPPED -> do 
                        liftRpc $ hideObj backendObj
                        liftRpc $ notifyComCitrixXenclientNetworkdomainNotifyBackendStateChanged (fromString backendObj) eNDVM_STATUS_STOPPED
                        liftIO $ updateNwsState appState uuidU Closed
                        liftRpc $ do
                            cleanupSlaveObjs appState uuidU domidI 
                            configClearFirewallRules uuidU
                         
        | otherwise -> return ()
    where backendObj = networkBackendObj uuidU
          uuidU = fromString uuid
          domidI = fromIntegral domid

listNetworkBackends :: App ([String])
listNetworkBackends = do 
    knownSlaves <- M.elems <$> appStateSlaveInfo
    return $ concat $ map runningBackends knownSlaves

    where runningBackends slaveObj = if (networkSlaveState  slaveObj == Initialized)
                                        then (networkSlaveObj slaveObj):[]
                                        else [] 

getNetworkBackend :: String -> App String
getNetworkBackend nw = do
    appState <- getAppState
    nwdNws <- liftIO $ getNetworks appState
    let nws = catMaybes $ map (nwLookup nw) nwdNws

    if (null nws)
       then liftRpc $ networkGetConfig nw eNW_PROP_UUID
       else return $ uuidStr $ head $ nws

    where 
        nwLookup nw nwdObjs = case (M.lookup nw nwdObjs) of 
                                   Just info -> Just $ networkBackendUuid info
                                   otherwise -> Nothing

listNetworks :: App ([(M.Map String String)])
listNetworks = do
    appState <- getAppState
    nwdNws <- liftIO $ getNetworks appState
    nws <- mapM listSlaveNetworks nwdNws
    configNws <- listConfiguredNetworks

    let allNws :: (M.Map String (M.Map String String)) = M.union (M.unions $ concat nws) (M.unions configNws)

    return $ M.elems $ allNws

    where
        listConfiguredNetworks :: App [(M.Map String (M.Map String String))]
        listConfiguredNetworks = do nws <- mapM listNetworksOfType networkTypes
                                    return $ concat nws

        listNetworksOfType :: String -> App [(M.Map String (M.Map String String))]
        listNetworksOfType nwType = do
             nws <- liftRpc $ configGetNetworksOfType nwType
             mapM (listNetwork nwType) (M.toList nws)

        listNetwork :: String -> (String, String) -> App (M.Map String (M.Map String String))
        listNetwork nwType (nwId, nwInfo) = do
             initializedUuids <- M.keys <$> initializedSlaves
             if ((fromString uuid) `elem` initializedUuids)
                then return M.empty
                else do
                    case  () of 
                        _ | (nwType == eNETWORK_TYPE_WIRED) -> do 
                                      bridgedNw <- nw eCONNECTION_TYPE_BRIDGED (nwdObj eCONNECTION_TYPE_BRIDGED)  nwType
                                      sharedNw <- nw eCONNECTION_TYPE_SHARED (nwdObj eCONNECTION_TYPE_SHARED) nwType
                                      return $ M.union bridgedNw sharedNw
                          | (nwType == eNETWORK_TYPE_WIFI) ->
                             nw eCONNECTION_TYPE_SHARED (nwdObj eCONNECTION_TYPE_SHARED) nwType
                          | (nwType == eNETWORK_TYPE_MODEM) -> 
                             nw eCONNECTION_TYPE_SHARED (nwdObj eCONNECTION_TYPE_SHARED) nwType
                          | (nwType == eNETWORK_TYPE_INTERNAL) ->
                             nw eCONNECTION_TYPE_SHARED (nwdObj "") nwType
                          | (nwType == eNETWORK_TYPE_ANY) ->
                             nw eCONNECTION_TYPE_SHARED (nwdObj "") eNETWORK_TYPE_UNKNOWN
                          | (nwType == eNETWORK_TYPE_VPN) ->
                             nw eCONNECTION_TYPE_UNKNOWN (nwdObj "") nwType
        
	     where 
                    uuidMaybe = getConfigProperty nwInfo eNW_PROP_UUID
                    uuid = fromMaybe "" uuidMaybe
                    nwdObj connType = dbusObjFromNwType nwId nwType connType
                    nw mode object nwType = liftRpc $ do 
                        vm <- (vmName uuidMaybe)
                        return $ M.fromList [((nwdObj mode), nwDetails vm)]
                        where 
                                nwDetails vm = M.fromList [("mode", mode)
                                                             , ("mac", mac)
                                                             , ("driver", "")
                                                             , ("object", object)
                                                             , ("type", nwType)
                                                             , ("backend_domain", "")
                                                             , ("backend_vm", vm)
                                                             , ("label", label)]
                                vmName :: (Maybe String) -> Rpc String
				vmName Nothing = return ""
		                vmName (Just uuid) = getVmNameFromUuid $ fromString uuid

		                label = fromMaybe "" $ getConfigProperty nwInfo eNW_PROP_LABEL
		                mac = fromMaybe "" $ getConfigProperty nwInfo eNW_PROP_MAC

        listSlaveNetworks :: (M.Map String NwsObjInfo) -> App [(M.Map String (M.Map String String))]
        listSlaveNetworks networks = mapM listEachNetwork $ M.toList networks

listEachNetwork :: (String, NwsObjInfo) -> App (M.Map String (M.Map String String))
listEachNetwork (nwdObj, nwInfo) = do
    appState <- getAppState
    let uuid = networkBackendUuid nwInfo
    backendInfo <- liftIO $ readMVar (nwsInfo appState)

    case M.lookup uuid backendInfo of
         Just info -> do let domid = networkBackendDomid info
                             slaveNwObj = nwsObj nwInfo 
                             backendObj = networkSlaveObj info 
                         liftRpc $ do 
                             vmName <- getVmNameFromUuid uuid
                             (_,_,_,nwConf) <- networkConfig nwdObj
                             getNwInfo domid uuid nwdObj slaveNwObj backendObj vmName nwConf
         otherwise -> return M.empty 

-- get network info
getNwInfo :: DomainId -> Uuid -> String -> String -> String -> String -> String -> Rpc (M.Map String (M.Map String String))
getNwInfo domid uuid extObj nwObj backend vmName nwConf = do
    connType <- withNws (NC.comCitrixXenclientNetworkConfigGetConnection slaveService nwObj)
    nwType <- withNws (NC.comCitrixXenclientNetworkConfigGetType slaveService nwObj)

    nwMac <- withNws (NC.comCitrixXenclientNetworkConfigGetMacAddress slaveService nwObj)
    nwDriver <- withNws (NC.comCitrixXenclientNetworkConfigGetDriver slaveService nwObj)

    let objType = case () of
                    _  | (nwType == eNETWORK_TYPE_ANY) -> eNETWORK_TYPE_UNKNOWN
                       | otherwise -> nwType

        mode = case () of
                  _ | (nwType == eNETWORK_TYPE_VPN) ->  eCONNECTION_TYPE_UNKNOWN
                    | otherwise -> connType

        label = fromMaybe "" (getConfigProperty nwConf eNW_PROP_LABEL)

    return $ M.fromList [(extObj, M.fromList [("mode", mode)
                                              , ("mac", nwMac)
                                              , ("driver", nwDriver)
                                              , ("object", extObj)
                                              , ("type", objType)
                                              , ("backend_domain", backend)
                                              , ("backend_vm", vmName)
                                              , ("label", label)])]

    where withNws = withNetworkSlave domid

-- check if we have any network connectivity
isNetworkingActive :: App (Bool)
isNetworkingActive = do
    domids <- slaveDomids 
    status <- anyM (domainConnectivity False) domids
    debug $ printf "Networking active: %s" (show status)
    return status

moveToNetwork :: String -> String -> App ()
moveToNetwork vif network = do
    appState <- getAppState
    case (domAndDevIdFromVif vif) of
         [gDomid, gDevid] -> void $ do
            if ((read gDomid :: Int32) == 0) 
               then void $ do
                    debug $ printf "Move dom0 vif to %s" network
                    dom0Networking <- liftRpc $ configDom0NetworkingEnabled
                    liftRpc $ configDom0SetNetwork network
                    when dom0Networking $ void $ do 
                        gMac <- liftIO $ fromJust <$> xsVifMac gDomid gDevid
                        nwState <- liftIO $ nwLookup network appState
                        moveVifToNetwork appState vif gDomid gDevid gMac nwState
               else void $ do
                    gVmObj <- liftRpc $ getVmFromDomid (read gDomid :: Int32)
                    debug $ printf "Moving %s vif to %s" (show gVmObj) network
                    unless (null gVmObj) $ do 
                        gMac <- liftRpc $ getVmNicMac gVmObj gDevid
                        nwState <- liftIO $ nwLookup network appState
                        moveVifToNetwork appState vif gDomid gDevid gMac nwState
         otherwise -> return ()
    where
        moveVifToNetwork appState vif gDomid gDevid gMac nwState = do
           case nwState of
                Nothing -> debug $ printf "Network not initialized yet-  %s" network
                Just nw -> void $ do 
                    newBackend <- liftIO $ fromJust <$> backendLookup (networkBackendUuid nw) appState
                    let newDomid = networkBackendDomid newBackend
                    curBackendInfo <- domainWithVif vif
                    debug $ printf "Move from %s(with vif %s) to %s(with network %s)" (show curBackendInfo) vif (show newBackend) network
                    case curBackendInfo of
                         [] -> void $ do
                                  mac <- liftRpc $ vifMac newDomid gDomid gMac nw
                                  void $ liftIO $ addVif newDomid gDomid gDevid gMac
                         y:_ -> void $ do
                             if (networkBackendDomid y == newDomid)
                                then moveVif newDomid gDomid gDevid vif nw
                                else moveVifToDomain (networkBackendDomid y) newDomid gDomid gDevid gMac

        addVif sDomid gDomid gDevid mac = system (xenopsAddVifCmd gDomid (show sDomid) mac gDevid) 
        delVif sDomid gDomid gDevid = system (xenopsDelVifCmd gDomid (show sDomid) gDevid) 
        disconnectVif sDomid gDomid gDevid =  xsSetVifDisconnect gDomid gDevid (show sDomid) "1"

        vifMac sDomid gDomid gMac nw = if ((read gDomid :: Int) == 0)
             then withNetworkSlave sDomid (NC.comCitrixXenclientNetworkConfigGetMacAddress slaveService (nwsObj nw))  >>= return
             else return gMac

        moveVifToDomain :: DomainId -> DomainId -> String -> String -> String -> App ()
        moveVifToDomain oDomid nDomid gDomid gDevid gMac= void $ liftIO $ do
            disconnectVif oDomid gDomid gDevid
            delVif oDomid gDomid gDevid
            addVif nDomid gDomid gDevid gMac

        moveVif :: DomainId -> String -> String -> String -> NwsObjInfo -> App ()
        moveVif sDomid gDomid gDevid vif nw = do
            debug $ printf  "Moving vif between networks within the same domain(%d)" sDomid
            liftIO $ disconnectVif sDomid gDomid gDevid
            liftRpc $ do
	        --withNetworkSlave sDomid (NWS.comCitrixXenclientNetworkslaveMoveVifToNetwork slaveService slaveRootObj vif (nwsObj nw))
                addVifToNetwork sDomid (networkBackendUuid nw) (read gDomid :: Int) vif (nwsObj nw)
                syncVifState sDomid (read gDomid) gDevid nw

        waitForVif cmd = loop =<< system cmd where
                loop ExitSuccess = return ()
                loop _ = threadDelay 1000 >> waitForVif cmd

{-----------------------------------------------------------------------------
 Network obj interfaces
------------------------------------------------------------------------------}
implementNetwork :: Uuid -> DomainId -> String -> String -> Bool -> Rpc ()
implementNetwork uuid domid nwsObj nwdObj transpBridging = rpcExpose (fromString nwdObj) . NS.interfaces $ NetworkServer {
          comCitrixXenclientNetworkConfigGetName = return nwdObj
          , comCitrixXenclientNetworkConfigGetBridge = withNws (NC.comCitrixXenclientNetworkConfigGetBridge slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetBackendUuid = return (uuidStr uuid)
          , comCitrixXenclientNetworkConfigGetActive = withNws (NC.comCitrixXenclientNetworkConfigGetActive slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetInterface = withNws (NC.comCitrixXenclientNetworkConfigGetInterface slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetMacAddress = withNws (NC.comCitrixXenclientNetworkConfigGetMacAddress slaveService nwsObj)
          , comCitrixXenclientNetworkConfigSetMacAddress = \_ -> return ()
          , comCitrixXenclientNetworkConfigGetDriver = withNws (NC.comCitrixXenclientNetworkConfigGetDriver slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetType = withNws (NC.comCitrixXenclientNetworkConfigGetType slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetConnection = withNws (NC.comCitrixXenclientNetworkConfigGetConnection slaveService nwsObj)
          --Another hack to make nm-applet show up as connected in transparent bridging state
          , comCitrixXenclientNetworkConfigGetNmState = if transpBridging then return 100 else withNws (NC.comCitrixXenclientNetworkConfigGetNmState slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetExtraInfo = withNws (NC.comCitrixXenclientNetworkConfigGetExtraInfo slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetNmManaged = withNws (NC.comCitrixXenclientNetworkConfigGetNmManaged slaveService nwsObj)
          , comCitrixXenclientNetworkConfigGetLabel = networkGetConfig nwdObj eNW_PROP_LABEL 
          , comCitrixXenclientNetworkConfigSetLabel = networkSetConfig nwdObj eNW_PROP_LABEL
          , comCitrixXenclientNetworkConfigGetNatPrefix = withNws (NC.comCitrixXenclientNetworkConfigGetNatPrefix slaveService nwsObj)
          , comCitrixXenclientNetworkConfigSetNatPrefix = networkSetConfig nwdObj eNW_PROP_NATPREFIX
          , comCitrixXenclientNetworkConfigure = \_ -> return ()
          , comCitrixXenclientNetworkJoin = \iface -> withNws (NC.comCitrixXenclientNetworkJoin slaveService nwsObj iface)
          , comCitrixXenclientNetworkLeave = \iface -> withNws (NC.comCitrixXenclientNetworkLeave slaveService nwsObj iface)
          , comCitrixXenclientNetworkIsConfigured = withNws (NC.comCitrixXenclientNetworkIsConfigured slaveService nwsObj)
          }

          where withNws = withNetworkSlave domid

networkGetConfig networkObj nwProperty = do
    (nwType, nwId, _, nwConfig) <- networkConfig networkObj
    let label = fromMaybe "" $ getConfigProperty nwConfig nwProperty
    return label

networkSetConfig networkObj property value =
    if (property `elem` networkProperties)
       then do
            (nwType, nwId, _, nwConfig) <- networkConfig networkObj
            let values = map ((fromMaybe "") . getConfigProperty nwConfig) networkProperties
                pairs = zip networkProperties values
                confMap = M.fromList pairs
            configSetNetwork nwType nwId $ M.insert property value confMap
       else debug $ printf "Cannot set %s for %s - config not supported" property networkObj

implementNetworkConfig :: String -> String -> String ->  Rpc ()
implementNetworkConfig nwType connType nwIndex = do
    debug $ printf "implementNetworkConfig %s %s %s - %s" nwType connType nwIndex nwdObj
    rpcExpose (fromString nwdObj) . NS.interfaces $ NetworkServer {
        comCitrixXenclientNetworkConfigGetName = return nwdObj
        , comCitrixXenclientNetworkConfigGetBridge = return ""
        , comCitrixXenclientNetworkConfigGetBackendUuid = networkGetConfig nwdObj eNW_PROP_UUID
        , comCitrixXenclientNetworkConfigGetActive = return False
        , comCitrixXenclientNetworkConfigGetInterface = return ""
        , comCitrixXenclientNetworkConfigGetMacAddress = networkGetConfig nwdObj eNW_PROP_MAC
        , comCitrixXenclientNetworkConfigSetMacAddress = \_ -> return ()
        , comCitrixXenclientNetworkConfigGetDriver = return ""
        , comCitrixXenclientNetworkConfigGetType = return nwType
        , comCitrixXenclientNetworkConfigGetConnection = return connType
        , comCitrixXenclientNetworkConfigGetLabel = networkGetConfig nwdObj eNW_PROP_LABEL
        , comCitrixXenclientNetworkConfigSetLabel = networkSetConfig nwdObj eNW_PROP_LABEL
        , comCitrixXenclientNetworkConfigGetNatPrefix = return ""
        , comCitrixXenclientNetworkConfigSetNatPrefix = networkSetConfig nwdObj eNW_PROP_NATPREFIX
        , comCitrixXenclientNetworkConfigGetNmState = return eNM_STATE_UNKNOWN
        , comCitrixXenclientNetworkConfigGetExtraInfo = backendInfo
        , comCitrixXenclientNetworkConfigGetNmManaged = return False
        , comCitrixXenclientNetworkConfigure = \_ -> return ()
        , comCitrixXenclientNetworkJoin = \_ -> return ()
        , comCitrixXenclientNetworkLeave = \_ -> return ()
        , comCitrixXenclientNetworkIsConfigured = return False
        }
 
    where 
        backendInfo = case () of
                         _ | (nwType == eNETWORK_TYPE_VPN) -> do
                                 networkGetConfig nwdObj eNW_PROP_BACKEND >>= f 
                           | otherwise -> return M.empty
                           where 
                              f ""      = return M.empty
                              f backend = return $ M.fromList [("backend-uuid", backend)]

        eNM_STATE_UNKNOWN = 0    
        nwdObj = dbusObjFromNwType nwIndex nwType connType

exportNetworkDaemonConfig :: Rpc ()
exportNetworkDaemonConfig = mapM_ exportNetworksOfType networkTypes

exportNetworksOfType nwType = do
    nws <- configGetNetworksOfType nwType
    debug $ printf "export %s networks - %s" (map toLower nwType) (show nws)
    mapM_ (exportNetwork nwType) (M.keys nws)

exportNetworkWithName nwName = do
    (nwTypeL, nwId, connTypeL, nwInfo) <- networkConfig nwName
    if (null nwInfo)
       then return ()
       else implementNetworkConfig nwTypeL connTypeL nwId

exportNetwork :: String -> String -> Rpc ()
exportNetwork nwType nwIndex = do
    case () of
	_ | (nwType == eNETWORK_TYPE_WIRED) -> do 
		 implementNetworkConfig nwType eCONNECTION_TYPE_BRIDGED nwIndex
		 implementNetworkConfig nwType eCONNECTION_TYPE_SHARED nwIndex
	  | (nwType == eNETWORK_TYPE_WIFI || nwType == eNETWORK_TYPE_MODEM) -> do 
		 implementNetworkConfig nwType eCONNECTION_TYPE_SHARED nwIndex
	  | otherwise -> do 
		 implementNetworkConfig  nwType eCONNECTION_TYPE_UNKNOWN nwIndex

createNetwork :: String -> Int32 -> String -> Rpc String
createNetwork nwTypeC id config = do
    if (notElem nwType networkTypes)
       then return $ printf "Invalid network type. Supported types - %s" (show $ unwords networkTypes)
       else do
            fixedIds <- map (read :: String -> Int32)  <$> configNetworksList (nwType)
            if (notElem id fixedIds)
               then do 
                    let values = map ((fromMaybe "") . getConfigProperty config) networkProperties
                        pairs = zip networkProperties values
                        confMap = M.fromList pairs
                        nwObjs = dbusObjsFromNwType nwType idS
                    configSetNetwork  nwType idS confMap
                    mapM_ hideObj  nwObjs
                    exportNetwork nwType idS
                    return $ unlines nwObjs  
               else return $ printf "Network with that id already exists."
    where nwType = map toLower nwTypeC
          idS = show id
