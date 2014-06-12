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

{-# LANGUAGE ScopedTypeVariables #-}
module NetworkInterface where

import Data.Int
import Data.List
import Data.String
import Data.Word
import Data.Bits
import Data.List ((\\), nub)
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Maybe

import Text.Printf (printf)
import Text.Regex.Posix

import Directory

import System.FilePath
import System.Process
import System.Exit
import System.Directory (copyFile, createDirectoryIfMissing)
import System.IO

import Control.Monad
import Control.Monad.Error
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad.Reader
import Control.Applicative

import Tools.XenStore
import Tools.Process
import Tools.Log
import Tools.Text
import Tools.Misc
import Tools.File

import App
import Utils
import Error
import NetworkManager
import NetworkUtils
import NetworkForwarding
import NetworkFirewall
import Rpc
import Rpc.Core
import Rpc.Autogen.NetworkServer as NS
import Rpc.Autogen.NetworkConst
import Rpc.Autogen.NetworkNotify
import Rpc.Autogen.NetworkSlaveNotify
import Rpc.Autogen.NetworkSlaveConst

import Rpc.Autogen.NmManagerClient
import Rpc.Autogen.NmDeviceClient
import Rpc.Autogen.NmActiveConnectionClient

anyBridge = "brany"
anyNwObj = "/any"

isWired :: NetworkObj -> Bool
isWired network = network =~ "wired" :: Bool

isWireless :: NetworkObj -> Bool
isWireless network = network =~ "wireless" :: Bool

isInternal :: NetworkObj -> Bool
isInternal network = network =~ "internal" :: Bool

isMobile :: NetworkObj -> Bool
isMobile network = network =~ "mobile" :: Bool

isSharedNetwork :: NetworkObj -> Bool
isSharedNetwork network = network =~ "shared" :: Bool

bridgeForWired useIfindexForEth0 iface ifindex brType =
    case (useIfindexForEth0, (iface == "eth0")) of
         (False, True) -> brType
         otherwise -> brType ++ ifindex

bridgeForWireless useIfindexForWiFi nwIndex ifindex =
    case (useIfindexForWiFi, nwIndex) of
         (False, 0) -> "brwireless"
         otherwise -> "brwireless" ++ ifindex

bridgeForMobile useIfindexForWiFi iface ifindex =
    case (useIfindexForWiFi, (iface == "ppp0")) of
         (False, True) -> "brwireless"
         otherwise -> "brwireless" ++ ifindex

lookupNetwork :: String -> App (Maybe NetworkInfo)
lookupNetwork network = do
    networks <- case (isWired network, isWireless network, isMobile network, isInternal network) of
                     (_,_,_,True) -> stateInternalNetworks
                     (_,_,True,_) -> stateMobileNetworks
                     (_,True,_,_) -> stateWirelessNetworks
                     (True,_,_,_) -> stateWiredNetworks
                     otherwise -> return M.empty
    return $ lookupNw network networks
    where
        lookupNw :: String -> (M.Map NetworkObj NetworkInfo) -> Maybe NetworkInfo
        lookupNw network knownNws = M.lookup network knownNws

getMaxIndex :: (M.Map NetworkObj NetworkInfo) -> Int 
getMaxIndex existingNws =
    maximum (-1 : map match (M.keys existingNws))
    where match str = let (_,index,_,_) = str=~ "[0-9]+" :: (String, String, String, [String])
                      in read index

stopNetworkSlave :: App ()
stopNetworkSlave = do
    liftIO $ cleanupNATRules
    liftIO $ stopNetworkManager
 
    mapM_ cleanupNws [stateWiredNetworks, stateWirelessNetworks, stateMobileNetworks, stateInternalNetworks] 
    liftIO $ exitWith ExitSuccess 

    where
        cleanupNws knownNws = do
             nws <- M.toList <$> knownNws  
             mapM_ removeNws nws

        removeNws (nwObj, nwInfo)  = removeNw nwObj (nwBridge nwInfo)

isValidSubnetRange :: String -> Maybe [String] 
isValidSubnetRange subnetRange = 
    case words subnetRange of 
         [start,end,ip,netmask] -> if all validIP [start,end,ip] && validNM netmask
                    then Just [start,end,ip,netmask]
                    else Nothing 
         other -> Nothing 
    where 
         -- TODO - correct these later by using haskell module (Data.IP)
         validIP ip = (ip =~ "(((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3})(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)" :: Bool)
         validNM nm = (nm =~ "([1-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])){3}" :: Bool) 

stateNewNetworks = liftIO . readMVar . newNetworkNames =<< getAppState

stateInitialized = liftIO . readMVar . slaveInitialized =<< getAppState

stateWiredNetworks :: App (M.Map NetworkObj NetworkInfo)
stateWiredNetworks = liftIO . readMVar . wiredNetworks =<< getAppState

stateWirelessNetworks :: App (M.Map NetworkObj NetworkInfo)
stateWirelessNetworks = liftIO . readMVar . wirelessNetworks =<< getAppState

stateInternalNetworks :: App (M.Map NetworkObj NetworkInfo)
stateInternalNetworks = liftIO . readMVar . internalNetworks =<< getAppState

stateMobileNetworks :: App (M.Map NetworkObj NetworkInfo)
stateMobileNetworks = liftIO . readMVar . mobileNetworks =<< getAppState

stateAllNetworks :: App (M.Map NetworkObj NetworkInfo)
stateAllNetworks = getAppState >>= \s -> liftIO $ do
        a <- readMVar (wiredNetworks s)
        b <- readMVar (wirelessNetworks s)
        c <- readMVar (mobileNetworks s)
        d <- readMVar (internalNetworks s)
        return $
          a `M.union` b `M.union` c `M.union` d

notifyAddedNetwork :: [NetworkObj] -> App ()
notifyAddedNetwork networks = do
    initialized <- stateInitialized
    when initialized $
         liftRpc $ notifyComCitrixXenclientNetworkslaveNotifyNetworkAdded (fromString nwsRootObj) networks

notifyRemovedNetwork :: [NetworkObj] -> App ()
notifyRemovedNetwork networks = do
    initialized <- stateInitialized
    when initialized $
         liftRpc $ notifyComCitrixXenclientNetworkslaveNotifyNetworkRemoved (fromString nwsRootObj) networks

initNetworkSlave :: App ()
initNetworkSlave = void $ do
    debug "Initializing network slave"
    appState <- getAppState
    let confMVar = newNetworkNames appState
    liftIO $ do 
        newConf <- doesFileExist ("/etc" </> "network-daemon" </> "new-config")
        if newConf == True 
            then do debug "Enabling new network-daemon config"
                    modifyMVar_ confMVar (\_ -> return True)
            else debug "Disabling network-daemon config"
        initRtTables
        initUnreachRoutingTable
        initNATRules

    initNetworks

    liftIO $ do 
        enableIpForwarding
        -- Disable netfilter on the bridge to prevent bridged traffic from passing through iptables rules
        configureBridgeFiltering
        disableReversePathFilter "all"

    liftRpc $ do
      interceptNmSignals appState

configureBridgeFiltering = do
    configGetBridgeFiltering >>= \filterTraffic -> 
        if (filterTraffic == True) then enableBridgeNetfilter else disableBridgeNetfilter

initNetworks :: App ()
initNetworks = do
    nwIfs <- liftIO $ listNetworkInterfaces
    mapM_ (silent . scanNwInterface) nwIfs

--    initInternalNw
    initAnyNw
    where
      -- masks io exceptions
      silent action = do
        s <- getAppState
        c <- liftRpc rpcGetContext
        liftIO $ (void . rpc c $ runApp s action)
                   `E.catch` (\(ex :: E.SomeException) -> return())

scanNwInterface :: FilePath -> App ()
scanNwInterface iface = do
    isPhy <- liftIO $ isPhyInterface iface
    isSerial <- liftIO $ isSerialInterface iface
    isWireless <- liftIO $ isWirelessInterface iface

    case (isPhy, isSerial, isWireless) of
         (_, True, _) -> initMobileNetwork iface
         (False, False, _) -> return ()
         (_, _, True) -> initWirelessNetwork iface
         otherwise -> initWiredNetwork iface

initAnyNw :: App ()
initAnyNw = do
    implementNetwork anyNwObj "" anyBridge "" eNETWORK_TYPE_ANY eCONNECTION_TYPE_SHARED

    nwsMVar <- anyNetwork <$> getAppState
    liftIO $ swapMVar nwsMVar (anyNwObj)
    return ()

createInternalNw :: Int -> App ()
createInternalNw idx = do
    debug ("createInternalNw  - " ++ (show idx))
    implementNetwork internalNwObj "" internalBridge "" eNETWORK_TYPE_INTERNAL eCONNECTION_TYPE_SHARED
    nwsMVar <- internalNetworks <$> getAppState
    existingNws <- liftIO $ takeMVar nwsMVar
    let newNw = M.singleton internalNwObj  NetworkInfo{nwIndex=idx, nwInterface="", nwBridge=internalBridge}
    liftIO $ putMVar nwsMVar ( newNw `M.union` existingNws )
    return ()

    -- TODO Emit network_added signal for NWD to start configuring (Only after the slave is fully initialized)

    where
        internalNwObj = "/internal/" ++ show idx
        internalBridge = "brinternal" ++ show idx

initMobileNetwork :: String -> App ()
initMobileNetwork iface = do
    debug ("Configure mobile network  - " ++ (show iface))
    nwsMVar <- mobileNetworks <$> getAppState
    existingNws <- liftIO $ takeMVar nwsMVar
    ifindex <- liftIO $ interfaceIndex iface

    -- Hack to replicate xenclient-network script behaviour
    newNwConf <- stateNewNetworks

    let index =  1 + getMaxIndex existingNws
        indexStr = show index
        nwId = "/mobile/" ++ indexStr
        sharedNw = nwId ++ "/shared"
        --brshared = "brwireless" ++ ifindex
        brshared = bridgeForMobile newNwConf iface ifindex 

    implementNetwork sharedNw iface brshared iface eNETWORK_TYPE_MODEM eCONNECTION_TYPE_SHARED

    let srNw = M.singleton sharedNw  NetworkInfo{nwIndex=index, nwInterface=iface, nwBridge=brshared}

    liftIO $ putMVar nwsMVar ( srNw `M.union` existingNws )

    notifyAddedNetwork $ sharedNw:[]

initWirelessNetwork :: String -> App ()
initWirelessNetwork iface = do
    debug ("Configure wireless network  - " ++ (show iface))
    nwsMVar <- wirelessNetworks <$> getAppState
    existingNws <- liftIO $ takeMVar nwsMVar
    ifindex <- liftIO $ interfaceIndex iface

    -- Hack to replicate xenclient-network script behaviour
    newNwConf <- stateNewNetworks

    let index =  1 + getMaxIndex existingNws
        indexStr = show index
        nwId = "/wireless/" ++ indexStr
        sharedNw = nwId ++ "/shared"
        --brshared = "brwireless" ++ ifindex
        brshared = bridgeForWireless newNwConf index ifindex 

    liftIO $ disableReversePathFilter iface 
    implementNetwork sharedNw iface brshared iface eNETWORK_TYPE_WIFI eCONNECTION_TYPE_SHARED

    let srNw = M.singleton sharedNw  NetworkInfo{nwIndex=index, nwInterface=iface, nwBridge=brshared}

    liftIO $ putMVar nwsMVar ( srNw `M.union` existingNws )

    notifyAddedNetwork $ sharedNw:[]

initWiredNetwork :: String -> App ()
initWiredNetwork iface = do
    debug ("Configure wired network  - " ++ (show iface))
    nwsMVar <- wiredNetworks <$> getAppState
    existingNws <- liftIO $ takeMVar nwsMVar
    ifindex <- liftIO $ interfaceIndex iface

    -- Hack to replicate xenclient-network script behaviour
    newNwConf <- stateNewNetworks

    let index =  1 + getMaxIndex existingNws
        indexStr = show index
        nwId = "/wired/" ++ indexStr
        bridgedNw = nwId ++ "/bridged"
        sharedNw = nwId ++ "/shared"

        brbridged = bridgeForWired newNwConf iface ifindex "brbridged"
        brshared = bridgeForWired newNwConf iface ifindex "brshared"

    initialized <- stateInitialized
    when initialized $ 
        liftIO $ configureBridgedNetwork brbridged iface eNETWORK_TYPE_WIRED ""

    implementNetwork bridgedNw iface brbridged "" eNETWORK_TYPE_WIRED eCONNECTION_TYPE_BRIDGED
    implementNetwork sharedNw iface brshared brbridged eNETWORK_TYPE_WIRED eCONNECTION_TYPE_SHARED

    let brNw = M.singleton bridgedNw  NetworkInfo{nwIndex=index, nwInterface=iface, nwBridge=brbridged}
        srNw = M.singleton sharedNw  NetworkInfo{nwIndex=index, nwInterface=iface, nwBridge=brshared}

    liftIO $ putMVar nwsMVar (brNw `M.union` srNw `M.union` existingNws)

    notifyAddedNetwork $ bridgedNw:sharedNw:[]

implementNetwork :: NetworkObj -> String -> String -> String -> String -> String -> App ()
implementNetwork nwObj interface bridge sharedOutIf nwType connectionType  = do
    debug ("Export - " ++ (show nwObj))
    appState <- getAppState
 
    liftRpc $ rpcExpose (fromString nwObj) . NS.interfaces $ NetworkServer {
          comCitrixXenclientNetworkConfigGetName = return nwObj
          , comCitrixXenclientNetworkConfigGetBridge = return bridge
          , comCitrixXenclientNetworkConfigGetBackendUuid = return ""
          , comCitrixXenclientNetworkConfigGetActive = runApp appState $ networkCarrier bridge interface nwType connectionType
          , comCitrixXenclientNetworkConfigGetInterface = return interface
          , comCitrixXenclientNetworkConfigGetMacAddress = liftIO $ networkMac bridge interface nwType
          , comCitrixXenclientNetworkConfigSetMacAddress = \mac -> liftIO $ setNwMacAddress bridge interface connectionType mac
          , comCitrixXenclientNetworkConfigGetDriver = liftIO $ networkDriver bridge interface nwType
          , comCitrixXenclientNetworkConfigGetType = return nwType
          , comCitrixXenclientNetworkConfigGetConnection = return connectionType
          , comCitrixXenclientNetworkConfigGetNmState = runApp appState $ networkState bridge interface nwType connectionType
          , comCitrixXenclientNetworkConfigGetExtraInfo = runApp appState $ networkInfo bridge interface nwType connectionType
          , comCitrixXenclientNetworkConfigGetLabel = return ""
          , comCitrixXenclientNetworkConfigSetLabel = \_ -> return ()
          , comCitrixXenclientNetworkConfigGetNatPrefix = liftIO $ getNetworkSubnet bridge nwType connectionType
          , comCitrixXenclientNetworkConfigSetNatPrefix = \_ -> return ()
          , comCitrixXenclientNetworkConfigGetNmManaged = runApp appState $ networkNmManaged bridge interface nwType connectionType
          , comCitrixXenclientNetworkConfigure = \ipconfig -> liftIO $ configureNetwork bridge interface sharedOutIf nwType connectionType ipconfig
          , comCitrixXenclientNetworkJoin = \vif -> liftIO $ networkJoin nwObj bridge vif
          , comCitrixXenclientNetworkLeave = \vif -> liftIO $ networkLeave nwObj bridge vif
          , comCitrixXenclientNetworkIsConfigured = liftIO $ networkConfigured bridge interface sharedOutIf nwType connectionType
          }

configureInternalNetwork :: String -> IO ()
configureInternalNetwork bridge = do
    checkAndAddBridge bridge
    spawnShell $ printf "ip link set %s address fe:ff:ff:ff:ff:ff" bridge
    readProcessOrDie "ifconfig" [bridge, "0.0.0.0", "up", "promisc"] []
    return ()

doesNdvmStorageExist = doesDirectoryExist "/storage/ndvm"
                         
configureAnyNetwork brshared subnetRange = do
    debug $ printf "configureAnyNetwork :  %s" subnetRange

    case isValidSubnetRange subnetRange of
        Just [dhcpStart, dhcpEnd, inputIfIP, netmask] -> do
            checkAndAddBridge brshared
            readProcess_closeFds "ip" ["link", "set", brshared, "address", "fe:ff:ff:ff:ff:ff"] []
            readProcessOrDie  "ifconfig" [brshared, inputIfIP,  "up", "promisc"] []
            readProcessOrDie  "ifconfig" [brshared, inputIfIP, "netmask", netmask, "up"] []
            startDNSMasqServer brshared Nothing inputIfIP dhcpStart dhcpEnd

        otherwise -> warn "Not valid format - So, not configuring the network."

configureSharedNetwork brshared interface outputIf nwType subnetRange = do
    debug $ printf "configureSharedNetwork : %s %s %s %s" brshared interface nwType subnetRange

    -- TODO Restrict this in the dbus interface
    case isValidSubnetRange subnetRange of
        Just [dhcpStart, dhcpEnd, inputIfIP, netmask] -> do
            ifindex <- interfaceIndex outputIf
 
            let routingTable = "rt-" ++ outputIf
            checkAndAddBridge brshared
            readProcess_closeFds "ip" ["link", "set", brshared, "address", "fe:ff:ff:ff:ff:ff"] []
            readProcessOrDie  "ifconfig" [brshared, "0.0.0.0",  "up", "promisc"] []
            readProcessOrDie  "ifconfig" [brshared, inputIfIP, "netmask", netmask, "up"] []
            addRoutingTableForInterface outputIf ifindex

            addRouteLookup ifindex routingTable brshared
 
            case (nwType == eNETWORK_TYPE_WIFI,  outputIf =~ "^wlan[0-9]+" :: Bool) of
                 (True, False) -> addNATMasqueradeRule outputIf
                 otherwise -> return ()

            startDNSMasqServer brshared (Just outputIf) inputIfIP dhcpStart dhcpEnd
            addForwardChain brshared outputIf interface
            -- TODO cleanup if any of the above goes wrong
        otherwise -> warn "Not a valid subnet range - So, not configuring the network."

configureBridgedNetwork :: String -> String -> String -> String -> IO ()
configureBridgedNetwork bridge interface nwType nwMac = do
    debug $ printf "configureBridgedNetwork : %s %s %s %s" bridge interface nwType nwMac
    checkAndAddBridge bridge
    disableReversePathFilter bridge
    readProcessOrDie "ifconfig" [interface, "0.0.0.0", "up", "promisc"] []
    ethMac <- if (null nwMac)
                 then interfaceMac interface
                 else if (isValidMac nwMac == True)
                      then do changeIfaceMac interface nwMac
                              return nwMac
                      else interfaceMac interface

    checkAndJoinBridge bridge interface >>= \success -> do
        case success of
             True -> do readProcessOrDie "brctl" ["setportprio", bridge, interface, "0"] []
                        readProcessOrDie "ip" ["link", "set", bridge, "address", ethMac] []
                        readProcessOrDie "ifconfig" [bridge, "0.0.0.0", "up"] []
                        initBridgedFiltering bridge interface
                        return ()
             False -> error $ printf "Configuration failure - failed to add %s to %s" interface bridge

setUpVifBeforeJoin bridge interface = unless (null bridge) $ void $ do
    debug  $ printf "Configure %s before joining it to %s" interface bridge
    spawnShell $ printf "ip link set %s down" interface
    spawnShell $ printf "ip link set %s arp off" interface
    spawnShell $ printf "ip link set %s multicast off" interface
    spawnShell $ printf "ip addr flush %s" interface

configureNetwork :: String -> String -> String -> String -> String -> String -> IO ()
configureNetwork bridge interface outputIf nwType connectionType configParams =  do
    case () of
        _ | (nwType == eNETWORK_TYPE_ANY) -> configureAnyNetwork bridge configParams
          | (nwType == eNETWORK_TYPE_INTERNAL) -> configureInternalNetwork bridge
          | (nwType == eNETWORK_TYPE_MODEM) -> return ()
          | (connectionType == eCONNECTION_TYPE_SHARED) -> configureSharedNetwork bridge interface outputIf nwType configParams
          | otherwise -> configureBridgedNetwork bridge interface nwType configParams

networkMac :: String -> String ->  String -> IO String
networkMac bridge interface nwType = do
    case () of
        _ | (nwType == eNETWORK_TYPE_INTERNAL || nwType == eNETWORK_TYPE_ANY) -> interfaceMac bridge
          | otherwise -> interfaceMac interface

networkDriver :: String -> String -> String -> IO String
networkDriver bridge interface nwType = do
    case () of
        _ | (nwType == eNETWORK_TYPE_MODEM) -> return "GSM Modem"
          | (nwType == eNETWORK_TYPE_WIRED || nwType == eNETWORK_TYPE_WIFI) -> driver interface
          | otherwise -> return "bridge"

    where
         driver interface = do 
            (exitCode,out,_) <- readProcessWithExitCode_closeFds "ethtool" ["-i", interface] []
            case exitCode of
                 ExitSuccess -> case (out `matchG` "driver:\\s*(.+)$") of
                                     [x] -> return x
                                     otherwise -> return ""
                 otherwise -> return ""

setNwMacAddress :: String -> String -> String -> String -> IO ()
setNwMacAddress bridge iface connectionType mac = do
    if (connectionType == eCONNECTION_TYPE_BRIDGED) 
        then do interfaceDown bridge
                changeIfaceMac iface mac
                changeIfaceMac bridge mac
                interfaceUp bridge
                return ()
        else return ()

networkDevice :: String -> String -> String -> String -> App (Maybe String)
networkDevice bridge interface nwType connectionType = do
    case () of   
       _  | (nwType == eNETWORK_TYPE_MODEM) -> return $ Just interface
          | (nwType == eNETWORK_TYPE_INTERNAL) -> return Nothing
          | (nwType == eNETWORK_TYPE_ANY) -> return Nothing
          | (nwType == eNETWORK_TYPE_WIFI) -> return $ Just interface
          | (nwType == eNETWORK_TYPE_WIRED && connectionType == eCONNECTION_TYPE_SHARED) -> do
                             newNwConf <- stateNewNetworks
                             ifindex <- liftIO $ interfaceIndex interface
                             return $ Just $ bridgeForWired newNwConf interface ifindex "brbridged"
          | otherwise -> return $ Just bridge


networkCarrier :: String -> String -> String -> String -> App Bool
networkCarrier bridge interface nwType connectionType = do
    case () of
      _ | (connectionType == eCONNECTION_TYPE_BRIDGED) -> liftIO $ interfaceCarrier bridge
        | otherwise -> nmNetworkState 
       
    where
    nmNetworkState = networkDevice bridge interface nwType connectionType >>= \nmIface -> 
                     case nmIface of
                          Nothing -> return True
                          Just d ->  liftRpc $ (objPathToStr <$> nmObjPath d) >>= f  where 
                                               f "" = return False
                                               f x  = nmCarrier x 

networkState :: String -> String -> String -> String -> App Word32
networkState bridge interface nwType connectionType = do
    interface <- networkDevice bridge interface nwType connectionType
    case interface of
         Nothing -> return eNM_DEVICE_STATE_UNMANAGED
         Just d -> liftRpc $ (objPathToStr <$> nmObjPath d) >>= f where
                     f "" = return eNM_DEVICE_STATE_UNKNOWN
                     f x = nmDeviceState x

networkNmManaged :: String -> String -> String -> String -> App Bool
networkNmManaged bridge interface nwType connectionType = do
    interface <- networkDevice bridge interface nwType connectionType
    case interface of
         Nothing -> return False
         Just d -> liftRpc $ (objPathToStr <$> nmObjPath d) >>= nmDeviceManaged
 
networkInfo :: String -> String -> String -> String -> App (M.Map String String)
networkInfo bridge interface nwType connectionType = do
    if (nwType == eNETWORK_TYPE_WIFI) 
       then liftRpc $ do
           nmDev <- objPathToStr <$> nmObjPath interface
           if (null nmDev)
              then return M.empty
              else do 
                  isActive <- (==eNM_DEVICE_STATE_ACTIVATED) <$> (nmDeviceState nmDev)
                  if isActive
                     then (objPathToStr <$> nmActiveAp nmDev) >>= f
                     else return M.empty
        else return M.empty
    where f "" = return M.empty
          f apObj = do
             ssid <- nmApSsid apObj
             mode <- nmApMode apObj
             freq <- nmApFrequency apObj
             wpaFlags <- nmApWpaFlags apObj
             rsnFlags <- nmApRsnFlags apObj
             hwAddr <- nmApHwAddress apObj
             br <- nmApMaxBitrate apObj
             strength <- nmApStrength apObj
             return $ M.fromList [(eACTIVE_AP_SSID, show ssid)
                                  , (eACTIVE_AP_MODE, show mode)
                                  , (eACTIVE_AP_FREQUENCY, show freq)
                                  , (eACTIVE_AP_STRENGTH, show strength)
                                  , (eACTIVE_AP_HWADDRESS, hwAddr)
                                  , (eACTIVE_AP_MAXBITRATE, show br)
                                  , (eACTIVE_AP_WPAFLAGS , show wpaFlags)
                                  , (eACTIVE_AP_RSNFLAGS , show rsnFlags)]


networkConfigured :: String -> String -> String -> String -> String -> IO Bool
networkConfigured bridge interface outputIf nwType connectionType = do
    exists <- bridgeExists bridge
    case exists of
         False -> return False
         True -> if (nwType == eNETWORK_TYPE_INTERNAL || nwType == eNETWORK_TYPE_ANY)
                    then return True 
                    else if (connectionType == eCONNECTION_TYPE_SHARED)
                               then sharedNetworkConfigured bridge interface  outputIf
                               else (memberOf interface) >>= \curBr -> return (curBr == bridge)

sharedNetworkConfigured :: String -> String -> String -> IO Bool
sharedNetworkConfigured bridge interface outputIf = do
    let rtTable = "rt-" ++ outputIf
    ifindex <- interfaceIndex outputIf

    rtTablesEntry <- routingTableForInterfaceExists outputIf ifindex
    rtExists <- routeLookupExists rtTable bridge 
    case (rtTablesEntry, rtExists) of
         (True, [True, True]) -> do debug $ "Route lookup exists for " ++ bridge
                                    pid <- dnsMasqPid bridge
                                    if null pid
                                       then return False
                                       else return True
         otherwise -> return False

networkJoin :: NetworkObj -> String -> String -> IO ()
networkJoin network bridge vif = do
    checkAndJoinBridge bridge vif >>= \success ->
        case success of
             False -> error $ printf "Failed to join interface %s to network %s" vif network
             True -> return ()

networkLeave :: NetworkObj -> String -> String -> IO ()
networkLeave network bridge vif = do
    checkAndLeaveBridge bridge vif >>= \success ->
        case success of
             False -> error $ printf "Failed to remove interface %s from network %s" vif network
             True -> return ()

removeNetworkWithInterface :: String -> App ()
removeNetworkWithInterface iface = do
    debug $ printf "Remove networks corresponding to %s"  iface
    wiredNetworksMVar <- wiredNetworks <$> getAppState
    wirelessNetworksMVar <- wirelessNetworks <$> getAppState
    mobileNetworksMVar <- mobileNetworks <$> getAppState

    mapM_ (checkAndDeleteNw iface) [wiredNetworksMVar, wirelessNetworksMVar, mobileNetworksMVar]
    return ()

    where
        checkAndDeleteNw :: String -> MVar (M.Map NetworkObj NetworkInfo) -> App Bool
        checkAndDeleteNw iface networksMVar = do
           networks <- liftIO $ takeMVar networksMVar 
            
           let (matchedNws, unmatchedNws) =  M.mapEither (matchNwIface iface) networks
           
           if M.null matchedNws
               then do liftIO $ putMVar networksMVar networks 
                       return True
               else do debug $ printf "Removing networks : %s" (show matchedNws)
                       mapM_ removeNws (M.toList matchedNws)
                       liftIO $ putMVar networksMVar (unmatchedNws)
                       return False

        removeNws (nwObj, nwInfo) = removeNw nwObj (nwBridge nwInfo)


matchNwIface :: String -> NetworkInfo -> Either NetworkInfo NetworkInfo
matchNwIface iface nwInfo  = if ( iface == nwInterface nwInfo )
				  then Left nwInfo
				  else Right nwInfo

matchNwBridge :: String -> NetworkInfo -> Either NetworkInfo NetworkInfo
matchNwBridge bridge nwInfo = if (bridge == nwBridge nwInfo)
                                 then Left nwInfo
                                 else Right nwInfo 

getNetworkWithProperty ::  MVar (M.Map NetworkObj NetworkInfo) -> (NetworkInfo -> Either NetworkInfo NetworkInfo) -> IO (M.Map NetworkObj NetworkInfo, M.Map NetworkObj NetworkInfo)
getNetworkWithProperty networksMVar matchFunc = do
    nwsMap <- liftIO $ readMVar networksMVar
    debug $ printf "getNetworkWithProperty %s " (show nwsMap)
    return $ M.mapEither (matchFunc) nwsMap

removeNw :: String -> String -> App ()
removeNw nw bridge = do 
    debug $ printf "Removing network %s" nw
    notifyRemovedNetwork $ nw:[]
    liftRpc $ rpcHide $ fromString nw
    liftIO $ removeBridge bridge
    
    when (isSharedNetwork nw) $ liftIO $ do
            pid <- dnsMasqPid bridge
            unless (null pid) $ stopDNSMasq bridge

configGetNmConfig :: String -> IO (String)
configGetNmConfig key = do
    (_, value,_) <- readProcessWithExitCode_closeFds "db-read-dom0" [path] []
    return $ strip value
    where path = "/" ++ key

nmManagesDevices :: IO Bool
nmManagesDevices = (/= "all") <$> configGetNmConfig eCONFIG_NM_UNMANAGED_DEVICES

-- update nm configuration files using dom-store
updateNmConfig = do
    -- networking and wireless are enabled by default. 
    networkingEnabled <- configGetNmConfig eCONFIG_NM_NETWORKING_ENABLED
    wirelessEnabled <- configGetNmConfig eCONFIG_NM_WIRELESS_ENABLED
    writeFile nmStateFile $ printf "[main]\nNetworkingEnabled=%s\nWirelessEnabled=%s\nWWANEnabled=false\nWimaxEnabled=false" (f networkingEnabled) (f wirelessEnabled)

    copyFile "/usr/share/xenclient/nm_scripts/NetworkManager.conf" nmConfFile
    m <- nmManagesDevices
    if not m
       then do phyMacs <- (mapM interfaceMac) =<< listPhyInterfaces
               appendFile nmConfFile $ printf "[keyfile]\nunmanaged-devices=%s\n" (concatMap (formatMac) phyMacs)
       else return () -- TODO when set unmanaged property is enabled for network objects, update NM conf accordingly

    -- create necessary folders needed by network manager 
    createDirectoryIfMissing False "/etc/NetworkManager/system-connections"
    createDirectoryIfMissing False "/etc/NetworkManager/dispatcher.d"

    where f x  = if (x == "false")
                 then "false"
                 else "true"
          formatMac mac = "mac:" ++ mac ++ ";"
      
startNetworkManager :: AppState -> Rpc ()
startNetworkManager s = do
    debug "Start NetworkManager"
    myDomid <- liftIO $ xsRead "domid"
    liftIO $ do
      case myDomid of
        Just domid -> do updateNmConfig
                         system "nm_sync.sh -n nm-connections -r"
                         system "nm_sync.sh -n seen-bssids -r"
                         return ()
        _ -> return ()

      system $ printf "chmod 600 %s/*" nmSysconDir
    -- run custom carrier monitoring path if network manager is not in charge of devices
    m <- liftIO nmManagesDevices
    if m
      then liftIO $ unmonitorCarriers s
      else monitorCarriers s

    liftIO . waitForNM $ maybe
      ( "NetworkManager" )
      ( const "NetworkManager" )
      myDomid
  where
    waitForNM cmd = loop =<< system cmd where
      loop ExitSuccess = return ()
      loop _ = threadDelay (10^5 * 2) >> waitForNM cmd

interceptNmSignals :: AppState -> Rpc ()
interceptNmSignals appState =  nmOnDeviceStateChanged (\x -> nmDevStateChanged appState x)

-- handle carrier changes w/o network manager cooperation
monitorCarriers :: AppState -> Rpc ()
monitorCarriers state = rpcGetContext >>= \ctx ->
  liftIO . void $ runInteractiveCommand "carrier-detect" >>= \ (_, stdout, _, h) ->
          do modifyMVar_ (carrierDetectProcess state) (\_ -> return $ Just h)
             hSetBuffering stdout NoBuffering
             forkIO . void $ consumeLines stdout (handleLine ctx)
    where
      handleLine ctx l = do
        case l `matchG` "^interface (.+) carrier ([0-9]+)" of
          [iface, carrier] -> handleFace ctx iface (read carrier)
          _                -> return ()

      handleFace ctx iface carrier = void . rpc ctx $ do
        nws <- runApp state $ nwForInterface iface
        debug $ "carrier change on " ++ iface ++ " = " ++ show carrier
        mapM_ (emitStateChanged nmstate) $ updateSlaveNws nws
        where
          nmstate = nmstateOfCarrier carrier

      nmstateOfCarrier 1 = eNM_DEVICE_STATE_ACTIVATED
      nmstateOfCarrier _ = eNM_DEVICE_STATE_DISCONNECTED

      nwForInterface iface = map fst . filter match . M.toList <$> stateAllNetworks where
        match (n,info) = nwBridge info == iface || nwInterface info == iface

      consumeLines h feed = go h where
          go h = continue =<< E.try (hGetLine h)
              where continue :: Either E.SomeException String -> IO ()
                    continue (Right l) = feed l >> go h
                    continue (Left _)  = return ()

unmonitorCarriers :: AppState -> IO ()
unmonitorCarriers s = do
  h <- swapMVar (carrierDetectProcess s) Nothing
  kill h
  where kill Nothing  = return ()
        kill (Just h) = terminateProcess h

-- handler for NM device state changed signals
nmDevStateChanged :: AppState -> String -> Rpc()
nmDevStateChanged appState nmDevObj = do 
    state <- nmDeviceState nmDevObj
    debug $ printf "nmDevStateChanged - %s state - %s" (show nmDevObj) (show state)
    nwObjs <- nwForNmDevice appState nmDevObj
     
    mapM_ (emitStateChanged state) $ updateSlaveNws nwObjs
 
emitStateChanged state nwObj = notifyComCitrixXenclientNetworkNotifyStateChanged (fromString nwObj) state
updateSlaveNws nws = case nws of
  [x] -> let newnw = replace "brbridged" "shared" x
         in nub $ newnw:nws
  otherwise -> nws
 
-- maps NM device to slave networks
nwForNmDevice appState nmDevObj = do 
    iface <- nmDeviceInterface nmDevObj
    devType <- nmDeviceType nmDevObj

    (nwObj, _) <- case () of
                       _ | (devType == eNM_DEVICE_TYPE_ETHERNET) -> liftIO $ getNetworkWithProperty (wiredNetworks appState) (matchNwBridge iface)
                         | (devType == eNM_DEVICE_TYPE_WIFI ) -> liftIO $ getNetworkWithProperty (wirelessNetworks appState) (matchNwIface iface)
                         | (devType == eNM_DEVICE_TYPE_MODEM ) -> liftIO $ getNetworkWithProperty (mobileNetworks appState) (matchNwIface iface)
                         | otherwise -> return (M.empty, M.empty)

    debug $ printf "nwForNmDevice iface:%s devType:%s nwObj:%s" (show iface) (show devType) (show nwObj)
    return $ M.keys nwObj


stopNetworkManager :: IO ()
stopNetworkManager = void $ do
    debug "Stop NetworkManager"
    getFileContents nmPidFile >>= \nmPid -> unless (null nmPid) $ void $ spawnShell $  "kill " ++ nmPid

moveVifToNetwork :: String -> NetworkObj -> App ()
moveVifToNetwork vif networkObj = 
    mapM_ (moveInterfaceToNetwork networkObj) $ nub $ vif:(tapIface vif):[]

    where 
        tapIface vif = replace "vif" "tap" vif

moveInterfaceToNetwork networkObj vif = do
    ifexists <- liftIO $ interfaceExists vif
    isVif <- liftIO $ isVifOrTunInterface vif
    nwInfo <- lookupNetwork networkObj
    debug  $ printf "moveInterfaceToNetwork %s %s %s" networkObj vif (show nwInfo)

    case (nwInfo, ifexists && isVif) of
         (_, False) -> debug $ printf "Move vif to network - Ignoring %s" vif
         (Just nwInfo, True) -> let newBridge = nwBridge nwInfo in
                                    moveVifToBridge newBridge vif
         (Nothing,_) -> if (networkObj == anyNwObj) 
                           then moveVifToBridge anyBridge vif
                           else error $ printf "Move vif to network failure - Unknown network %s" networkObj
    where 
        moveVifToBridge newBridge vif = liftIO $ do
                curBridge <- memberOf vif 
            --unless (curBridge == newBridge) $ do
                leaveBridge curBridge vif
                cleanupFirewallRules vif curBridge 
                setUpVifBeforeJoin newBridge vif
                applyFirewallRules vif newBridge
                joinBridge newBridge vif 
                debug $ printf "Move vif %s to %s network successful" vif newBridge


backendVifNotify :: String -> Word32 -> Word32 -> App ()
backendVifNotify vif backendDomId devid = do
    debug $ printf "backendVifNotify %s %d %d" vif backendDomId devid 
    liftIO $ interfaceUp vif
    let vifInfo = vif:(show backendDomId):(show devid):[]
    liftRpc $ notifyComCitrixXenclientNetworkslaveNotifyNewBackendVif (fromString nwsRootObj) vifInfo
 
refreshVifs :: App ()
refreshVifs = do
    nwIfs <- liftIO $ listNetworkInterfaces
    mapM_ (vifAdd) nwIfs

    where
        vifAdd iface = do
           case  (iface `matchG` "^vif([0-9]+)\\.([0-9]+)") of
                 [domidS, devidS] -> do 
		    let domid = read domidS :: Word32
			devid = read devidS :: Word32
                    debug $ printf "Add %s to it's network" (iface)
                    backendVifNotify iface domid devid
                 otherwise -> return ()

networkConnectivity :: App Bool
networkConnectivity = liftRpc $ do
    activeCons <- nmListActiveConnections 
    debug $ printf "Active connections - %s" (show activeCons)
    anyM isConnected activeCons

deviceState iface = (objPathToStr <$> nmObjPath iface) >>= nmDeviceState

isConnected :: ObjectPath -> Rpc Bool
isConnected nmConnection =  do 
    debug $ printf "Check if %s is connected" (show nmConnection)
    (== eNM_ACTIVE_CONNECTION_STATE_ACTIVATED) <$> ( nmActiveConnectionState (objPathToStr nmConnection))

listVifs :: App ([String])
listVifs = liftIO $ do
    nwIfs <- listNetworkInterfaces
    concat <$> mapM getVifs nwIfs

    where getVifs vif = isVifOrTunInterface vif >>= \isVif ->
                        if (isVif == True)
                           then return [vif]
                           else return [] 

getNetworkSubnet bridge nwType connectionType = 
    if (connectionType == eCONNECTION_TYPE_SHARED)
       then (interfaceSubnet bridge)
       else return ""
