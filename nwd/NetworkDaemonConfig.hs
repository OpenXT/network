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

module NetworkDaemonConfig where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Data.String
import Rpc
import Rpc.Core
import Rpc.Autogen.NetworkSlaveClient
import Tools.Db

import Control.Applicative
import Control.Monad
import Text.Printf
import Tools.Log
import System.FilePath ((</>))

import Directory
import NwTypes

configDom0IP="ip"
configDom0NM="netmask"
configDom0Domain="domain"
configDom0Dns="dns-servers"
configDom0Gw="default-gw"
dbNetworkDaemon = "/networkdaemon"
dbDom0Config = dbNetworkDaemon </> "dom0"
dbDom0Networking = dbDom0Config </> "networking-enabled"
dbDom0Network = dbDom0Config </> "network"
dbDom0IpConfig = dbDom0Config </> "ip-config"
dbNetworks = dbNetworkDaemon </> "networks"

ds = "/dom-store"
dsFirewallRules = "firewall-rules"
dsFirewallId = "id"
dsFirewallDirection = "direction"
dsFirewallRemoteIp = "remote-ip"
dsFirewallExtra = "extra"

eNW_PROP_UUID = "uuid"
eNW_PROP_MAC = "mac"
eNW_PROP_BACKEND = "backend"
eNW_PROP_LABEL = "label"
eNW_PROP_NATPREFIX = "nat-prefix"
eNW_PROP_NMMANAGED = "nm-managed"

networkProperties = [eNW_PROP_UUID, eNW_PROP_MAC, eNW_PROP_BACKEND, eNW_PROP_LABEL, eNW_PROP_NATPREFIX, eNW_PROP_NMMANAGED]


configDom0NetworkingEnabled :: Rpc Bool
configDom0NetworkingEnabled = liftIO $ not <$> doesFileExist "/config/system/dom0-networking-disabled"

configDom0Network :: Rpc String
configDom0Network = do
    dbMaybeRead dbDom0Network >>= f where f Nothing = return "/wired/0/bridged"
                                          f (Just v) = return v
  
configDom0SetNetwork :: String -> Rpc () 
configDom0SetNetwork network = dbWrite dbDom0Network network

configDom0IpConfig :: Rpc String
configDom0IpConfig = do
    dbMaybeRead dbDom0IpConfig >>= f where f Nothing = return "dhcp"
                                           f (Just v) = return v

configDom0SetIpConfig :: String -> Rpc ()
configDom0SetIpConfig mode = dbWrite dbDom0IpConfig mode


configDom0GetProperty :: String -> Rpc String
configDom0GetProperty property = do
    dbMaybeRead configDom0Property >>= f 
    where 
        f Nothing = return ""
	f (Just v) = return v

        configDom0Property = dbDom0Config </> property

configDom0SetProperty :: String -> String -> Rpc ()
configDom0SetProperty property value = dbWrite (dbDom0Config </> property) value

configGetNetworksOfType :: String -> Rpc (M.Map String String)
--configGetNetworksOfType property = do 
--    x<-dbRead (dbNetworks ++ "/" ++ property)
--    debug $ printf "dbRead %s - %s" (dbNetworks ++ "/" ++ property) (show x)
--    return x
configGetNetworksOfType property = M.fromList <$> pairs
                  where
                    nwType = map toLower property
                    x = dbNetworks </> nwType
                    pairs    = zip <$> ids <*> objs
                    ids      = map fromString <$> dbList x
                    id_paths = map (\id -> x </> id) <$> ids
                    objs     = mapM dbRead =<< id_paths

configSetNetwork :: String -> String -> (M.Map String String) -> Rpc ()
configSetNetwork nwType id config = do 
    let value = (nwDbConfig config)
    debug $ printf "configSetNetwork %s %s - %s" nwType id (show value)
    dbWrite (dbNetworks </> (map toLower nwType) </> id) (nwDbConfig config)

    where
        nwDbConfig :: (M.Map String String) -> String
        nwDbConfig config = concat $ intersperse "," $ map (nwDbVal config) networkProperties
 
         
        nwDbVal :: (M.Map String String) -> String -> String
        nwDbVal configMap key = let value = M.lookup key configMap 
                                in case value of
                                        Nothing -> ""
                                        Just v -> if null v 
                                                     then ""
                                                     else printf "%s=%s" key v

configGetNetwork :: String -> Rpc String
configGetNetwork nwId = dbRead (dbNetworks </> (map toLower nwId)) 

configNetworksList :: String -> Rpc [String]
configNetworksList property = dbList $ dbNetworks </> (map toLower property)

domstorePath uuid = ds </> show uuid

configClearFirewallRules :: Uuid -> Rpc ()
configClearFirewallRules ndvmUuid = dbRm dsFirewallRulesPath
    where dsFirewallRulesPath = (domstorePath ndvmUuid) </> dsFirewallRules

configSetFirewallRules :: Uuid -> Uuid -> String -> String -> String -> String -> Rpc ()
configSetFirewallRules ndvmUuid guestUuid ruleId direction remoteip extra = do
    dbWrite firewallDirection direction
    dbWrite firewallRemoteIp remoteip
    dbWrite firewallExtra extra

    where 
        domstoreFirewallPath =  (domstorePath ndvmUuid) </> dsFirewallRules </> show guestUuid </> ruleId
        firewallDirection = domstoreFirewallPath </> dsFirewallDirection
        firewallRemoteIp = domstoreFirewallPath </> dsFirewallRemoteIp
        firewallExtra = domstoreFirewallPath </> dsFirewallExtra 

configSetNdvmDomstore :: Uuid -> String -> Bool -> Rpc ()
configSetNdvmDomstore ndvmUuid key value = dbWrite propPath value 
    where propPath = (domstorePath ndvmUuid) </> key

configSetNdvmDomstoreS :: Uuid -> String -> String -> Rpc ()
configSetNdvmDomstoreS ndvmUuid key value = dbWrite propPath value 
    where propPath = (domstorePath ndvmUuid) </> key

configGetNdvmDomstore :: Uuid -> String -> Rpc (String)
configGetNdvmDomstore ndvmUuid key = dbRead propPath 
    where propPath = (domstorePath ndvmUuid) </> key

configGetNdvmDbConf :: Uuid -> String -> Rpc (String)
configGetNdvmDbConf ndvmUuid key = dbRead propPath
    where propPath = dbNetworkDaemon </> key </> (show ndvmUuid) 

configGetNdvmTransparentBridging :: Uuid -> Rpc (Bool)
configGetNdvmTransparentBridging uuid = configGetNdvmDomstore uuid "transparent-bridging" >>= \x -> return (x == "true") 
 
