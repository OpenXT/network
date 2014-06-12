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
module Main where

import Control.Applicative
import System
import System.IO
import Directory

import Rpc
import Rpc.Core
import App
import Tools.Log
import Tools.Process

import Data.String
import Data.Word
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import Data.Maybe

import Text.Printf
import Text.Regex.Posix
import System.FilePath.Posix
import System.Posix.Syslog
import System.Process (system)

import Control.Monad.Trans
import Control.Monad.Error
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

import Utils
import NetworkInterface
import NetworkUtils
import NetworkManager
import NetworkFirewall

import Rpc.Autogen.DbusClient
import Rpc.Autogen.NetworkSlaveServer as NWS
import Rpc.Autogen.NetworkSlaveNotify
import Rpc.Autogen.NetworkNmServer as NM

import Tools.XenStore

-- Main
main :: IO ()
main = do
          appState <- initAppState
          doService appState nwsService implementNetworkSlave
          return ()

-- Start the service and the implementations
doService appState service implementation = 
    withSyslog "network-slave" [] USER . rpcServe "com.citrix.xenclient.networkslave" $ \rpcContext ->
        do 
            r <- E.try $ do
                status <- rpc rpcContext $ runApp appState $ do 
                    implementation
                    liftRpc $ do onNotify "networkdaemon_up" (\_ _ _ _ -> do debug $ "Received a networkdaemon_up signal"
                                                                             notifyNetworkDaemon)
                                 notifyNetworkDaemon
                    -- live forever
                    liftIO . forever $ threadDelay (10^6 * 60)

                case status of
                     Left error -> fatal $ "error during initialisation " ++ show error
                     Right _    -> return ()
            case r of
                 Right () -> return ()
                 Left (ex :: E.SomeException) -> fatal $ "Caught error: " ++ show ex


onNotify :: String -> NotifyHandler -> Rpc ()
onNotify msgname action =
    let rule = matchSignal "com.citrix.xenclient.networkdaemon.notify" msgname
    in
      rpcOnSignal rule process
  where
    process senderName signal =
       do let sender = TL.unpack (strBusName senderName)
          domid <- orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" sender
          maybe_uuid <- getDomainUuid domid

          let uuid = case maybe_uuid of
                          Nothing -> domain0uuid
                          Just uuid1 -> uuid1

          action sender domid uuid $ signalArgs signal


notifyNetworkDaemon :: Rpc ()
notifyNetworkDaemon = do
    -- Re-emit networkslave_up signal for network daemon to start configuring
    -- slave networks
    notifyComCitrixXenclientNetworkslaveNotifyNetworkslaveUp (fromString nwsRootObj)

{-----------------------------------------------------------------------------
Network slave implementation
------------------------------------------------------------------------------}
implementNetworkSlave :: App ()
implementNetworkSlave =
    do
        -- scan sysfs for network interfaces, update the state, export network objs
        -- init each network by creating bridges, routing tables,
        initNetworkSlave
        implementNetworkSlaveInterfaces
        implementNetworkSlaveNMInterface
        initMVar <- slaveInitialized <$> getAppState
        liftIO $ swapMVar initMVar True
        return ()
    where


implementNetworkSlaveNMInterface = do 
    liftRpc $ rpcExpose (fromString "/nm") . NM.interfaces $ NetworkNmServer {
        comCitrixXenclientNetworkNmPopupNetworkMenu = emitPopupMenuSignal
        , comCitrixXenclientNetworkNmCloseNetworkMenu = emitCloseMenuSignal
    }

emitCloseMenuSignal = do
    (exitCode, out, _) <- liftIO $ readProcessWithExitCode_closeFds "dbus-send" ["--system", "--type=signal", "/", "com.citrix.xenclient.xui.close_network_menu"] []
    unless (exitCode == ExitSuccess) $ debug $ printf "Error when emitting close_network_menu signal: %s"  out
    return ()
   
emitPopupMenuSignal :: Word32 -> Word32 -> Rpc ()
emitPopupMenuSignal x_off y_off = do 
    (exitCode, out, _) <- liftIO $ readProcessWithExitCode_closeFds "dbus-send" ["--system", "--type=signal", "/", "com.citrix.xenclient.xui.popup_network_menu", x_arg, y_arg] []
    unless (exitCode == ExitSuccess) $ debug $ printf "Error when emitting popup_network_menu signal: %s"  out
    return ()

    where x_arg = "uint32:" ++ (show x_off)
          y_arg = "uint32:" ++ (show y_off)


{-----------------------------------------------------------------------------
Network slave interfaces
------------------------------------------------------------------------------}
implementNetworkSlaveInterfaces :: App ()
implementNetworkSlaveInterfaces = do
    appState <- getAppState
    liftRpc $ rpcExpose (fromString nwsRootObj) . NWS.interfaces $ NetworkSlaveServer {
        comCitrixXenclientNetworkslaveBackendVifNotify = \vif domid devid -> return ()
        , comCitrixXenclientNetworkslaveCreateInternalNetworks = \number -> runApp appState $ createInternalNetworks (fromIntegral number)
        , comCitrixXenclientNetworkslaveNetworkIfaceNotify = \action iface -> runApp appState $ networkInterfaceNotify action iface
        , comCitrixXenclientNetworkslaveRefreshVifs = runApp appState $  refreshVifs
        , comCitrixXenclientNetworkslaveShutdown = runApp appState $ stopNetworkSlave
        , comCitrixXenclientNetworkslaveStartNm = restartNetworkManager appState
        , comCitrixXenclientNetworkslaveIsInitialized = runApp appState stateInitialized
        , comCitrixXenclientNetworkslaveListNetworks = runApp appState listNetworks
        , comCitrixXenclientNetworkslaveListVifs = runApp appState listVifs
        , comCitrixXenclientNetworkslaveMoveVifToNetwork = \vif network -> runApp appState $ moveVifToNetwork vif network
        , comCitrixXenclientNetworkslaveNwConnectivity = runApp appState networkConnectivity
        , comCitrixXenclientNetworkslaveGetIcavmNetwork = runApp appState getIcavmNetwork 
        , comCitrixXenclientNetworkslaveNmState = runApp appState $ liftRpc $ nmState
    }

createInternalNetworks :: Int -> App ()
createInternalNetworks number = do
    mapM_ createInternalNw [ 0 .. (number - 1) ]

getIcavmNetwork :: App String
getIcavmNetwork = do
    appState <- getAppState
    liftIO $ readMVar (anyNetwork appState)

networkInterfaceNotify :: String -> String -> App ()
networkInterfaceNotify action interface = do
    case action of 
         "add" -> do 
                     exists <- liftIO $ checkForInterface interface 10 
                     debug $ printf "Received a udev notify for %s, exists : %s " interface (show exists)
                     isVif <- liftIO $ isVifOrTunInterface interface
                     isBridge <- liftIO $ isBridgeInterface interface
                     case (exists, isVif, isBridge) of
                          (_,_,True) -> debug $ printf "Ignoring udev notify for %s" interface
                          (True, False,_) -> do debug $ printf "Create network corresponding to %s" interface
                                                scanNwInterface interface
                          (True, True,_) -> do debug $ printf "Notify network daemon of a new backend vif %s" interface
                                               let [_, domid, devid] = interface `matchG` "(tap|vif)([0-9]+)\\.([0-9]+)"
                                               backendVifNotify interface (read domid) (read devid)
                          otherwise -> debug $ printf "Ignoring the udev notify event for %s" interface
         "remove" -> do if ((interface =~ "vif" :: Bool) || (interface =~ "tap" :: Bool))
                           then liftIO $ cleanupFirewallRules interface ""
                           else removeNetworkWithInterface interface
         other -> error $ printf "Udev network interface notify - %s action not supported" other
    where 
        checkForInterface interface attempts = do
            (exitCode,_,_) <- readProcessWithExitCode_closeFds "ifconfig" [interface] []
            if exitCode == ExitSuccess
               then return True
               else do
                    if attempts == 0
                       then return False
                       else do threadDelay 10000
                               checkForInterface interface (attempts -1)

restartNetworkManager :: AppState -> Rpc ()
restartNetworkManager s = do
    debug $ printf "Stop and start network manager"  
    liftIO stopNetworkManager
    startNetworkManager s

listNetworks :: App ([String])
listNetworks = do
    appState <- getAppState
    liftIO $ do 
        wiredNws <- readMVar (wiredNetworks appState)
        wirelessNws <- readMVar (wirelessNetworks appState)
        mobileNws <- readMVar (mobileNetworks appState)
        internalNws <- readMVar (internalNetworks appState)
        anyNw <- readMVar (anyNetwork appState)
        return ((M.keys wiredNws) ++ (M.keys wirelessNws) ++ (M.keys mobileNws) ++ (M.keys internalNws) ++ (anyNw:[]))
