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
module NetworkManager where

import Control.Applicative
import Control.Concurrent
import Data.Char
import Data.String
import Data.Maybe
import Data.Int
import Data.Word
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B
import System.FilePath ((</>))

import Text.Printf (printf)
import Tools.Log
import Tools.Misc
import Tools.XenStore
import Rpc
import Rpc.Core
import Utils

import Rpc.Autogen.NmManagerClient
import Rpc.Autogen.NmDeviceClient
import Rpc.Autogen.NmActiveConnectionClient
import Rpc.Autogen.NmDeviceWifiClient
import Rpc.Autogen.NmAccessPointClient

type Ssid = [Word8]
type Mac = String
nmService  = "org.freedesktop.NetworkManager"
nmObj      = "/org/freedesktop/NetworkManager"

nmEtc = "/etc/NetworkManager"
nmSysconDir = nmEtc </> "system-connections"
nmVar = "/var/lib/NetworkManager"
nmStateFile = nmVar </> "NetworkManager.state"
nmConfFile = nmEtc </> "NetworkManager.conf"
nmPidFile = "/var/run/NetworkManager.pid" 

nmOnDeviceStateChanged :: (String -> Rpc ()) -> Rpc ()
nmOnDeviceStateChanged action =
    let rule = matchSignal "org.freedesktop.NetworkManager.Device" "StateChanged"
    in
      rpcOnSignal rule process
  where
    process _ signal = let path   = signalPath signal
                           device = (show path)
                      in action device

nmOnWifiDevicePropertiesChanged :: (String -> Rpc ()) -> Rpc ()
nmOnWifiDevicePropertiesChanged action =
    let rule = matchSignal "org.freedesktop.NetworkManager.Device.Wireless" "PropertiesChanged"
    in
      rpcOnSignal rule process
  where
    process _ signal = let path   = signalPath signal
                           device = (show path)
                      in action device

objPathToStr :: ObjectPath -> String
objPathToStr = T.unpack . strObjectPath

nmObjPath :: String -> Rpc ObjectPath
nmObjPath iface = do 
    -- debug $ printf "nm device for %s " iface
    ifaceObj <- orgFreedesktopNetworkManagerGetDeviceByIpIface nmService nmObj iface
    -- debug $ printf "NM object path %s - %s" iface (show ifaceObj)
    return ifaceObj

nmState :: Rpc (Word32)
nmState = orgFreedesktopNetworkManagerGetState nmService nmObj

nmListActiveConnections :: Rpc ([ObjectPath])
nmListActiveConnections = orgFreedesktopNetworkManagerGetActiveConnections nmService nmObj

nmDeviceState nmDevObj = orgFreedesktopNetworkManagerDeviceGetState nmService nmDevObj
nmDeviceInterface nmDevObj = orgFreedesktopNetworkManagerDeviceGetInterface nmService nmDevObj
nmDeviceType nmDevObj = orgFreedesktopNetworkManagerDeviceGetDeviceType nmService nmDevObj
nmDeviceManaged nmDevObj = orgFreedesktopNetworkManagerDeviceGetManaged nmService nmDevObj
nmCarrier nmDevObj = (== eNM_DEVICE_STATE_ACTIVATED)  <$> (nmDeviceState nmDevObj)


nmActiveConnectionState nmConnection = orgFreedesktopNetworkManagerConnectionActiveGetState nmService nmConnection
nmActiveAp nmDevObj = orgFreedesktopNetworkManagerDeviceWirelessGetActiveAccessPoint nmService nmDevObj

nmApSsid nmApObj = orgFreedesktopNetworkManagerAccessPointGetSsid nmService nmApObj
nmApStrength nmApObj = orgFreedesktopNetworkManagerAccessPointGetStrength nmService nmApObj
nmApMode nmApObj = orgFreedesktopNetworkManagerAccessPointGetMode nmService nmApObj
nmApFrequency nmApObj = orgFreedesktopNetworkManagerAccessPointGetFrequency nmService nmApObj
nmApWpaFlags nmApObj = orgFreedesktopNetworkManagerAccessPointGetWpaFlags nmService nmApObj
nmApRsnFlags nmApObj = orgFreedesktopNetworkManagerAccessPointGetRsnFlags nmService nmApObj
nmApHwAddress nmApObj = orgFreedesktopNetworkManagerAccessPointGetHwAddress nmService nmApObj
nmApMaxBitrate nmApObj = orgFreedesktopNetworkManagerAccessPointGetMaxBitrate nmService nmApObj
