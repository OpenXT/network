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
module NetworkSlaveMethods where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Error (catchError)

import Data.Char
import Data.String
import Data.Maybe
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Map as M 
import qualified Data.Text.Lazy as TL
import Text.Printf
import Text.Regex.Posix
import Directory

import Rpc
import Rpc.Core
import App
import NwTypes
import Tools.Log
import Tools.Misc
import Tools.XenStore
import Tools.File
import System

import Rpc.Autogen.XenmgrVmClient
import Rpc.Autogen.XenmgrClient
import Rpc.Autogen.VmNicClient
import qualified Rpc.Autogen.NetworkSlaveClient as NWS
import Rpc.Autogen.NetworkConst

import Rpc.Autogen.DbusClient
import NetworkDaemonConfig

configPath = "/config"

-- Domain path in xenstore
domainXSPath :: DomainId -> XSPath
domainXSPath domid = printf "/local/domain/%d" domid

-- Get uuid of a domain using xenstore
getDomainUuid :: DomainId -> Rpc (Maybe Uuid)
getDomainUuid domid = do
  v <- liftIO . xsRead $ domainXSPath domid ++ "/vm"
  case v of
    Just ('/':'v':'m':'/':uuid_str) -> return . Just . fromString $ uuid_str
    _ -> return Nothing

withNetworkSlave :: DomainId -> Rpc a -> Rpc a
withNetworkSlave domid f = if (domid == 0)
                              then f
                              else rpcWithDomain domid f  

nwsOnSignal :: DomainId -> MatchRule -> NotifyHandler -> Rpc ()
nwsOnSignal domid rule action = do 
       previousContext <- rpcGetContext
       withNetworkSlave domid  $
              rpcOnSignal rule $ \sender signal ->
                  rpcLocalContext (\_ -> previousContext) (signalParser sender signal domid action)

signalParser :: BusName -> RpcSignal -> DomainId -> NotifyHandler -> Rpc ()
signalParser senderName signal domid action = do 
    let sender =  TL.unpack (strBusName senderName)
--    domid <- fromIntegral <$> orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" sender
    maybe_uuid <- getDomainUuid domid

    let uuid = case maybe_uuid of
                          Nothing -> domain0uuid
                          Just uuid1 -> uuid1

    debug $ printf "Received %s (%s %s) from %s" (show $ signalMember signal) (show $ signalPath signal) (show $ signalArgs signal) (sender)
    action sender domid uuid (signalPath signal) (signalArgs signal)

networkObjPath [nwObjV] = let Just nwObj = fromVariant nwObjV in nwObj
varToStr [nwObjV] = let Just nwObj = fromVariant nwObjV in nwObj

networkObjPaths :: [Variant] -> [String]
networkObjPaths [nwObjV] =  let Just nwObj = fromVariant nwObjV in nwObj
varToArrStr [nwObjV] =  let Just nwObj = fromVariant nwObjV in nwObj

xenmgrService = "com.citrix.xenclient.xenmgr"
xenmgrRootObj = "/"

vmNetworkConfig :: String -> String -> String -> Rpc (Maybe String)
vmNetworkConfig domid devid backendDomid = do
    if ((read domid :: Int) == 0)
       then Just <$> configDom0Network
       else liftIO $ xsVifNetwork domid devid backendDomid `catchError` (\ex -> getStubdomConfig domid devid backendDomid)

getStubdomConfig domid devid backendDomid= do 
    vmstubDomid <- liftIO $ domWithStubdom domid
    case vmstubDomid of
       Just stubdomid ->  xsVifNetwork stubdomid devid backendDomid
       other -> return Nothing

domWithStubdom domid = do 
    vmName <- xsRead $ printf "/local/domain/%d/name" domid
    case vmName of
         Just name -> 
             case (name `matchG` "stubdom-([0-9]+)") of
                  [x] -> return $ Just x
                  otherwise -> return Nothing

domAndDevIdFromVif vif = vif `matchG` "vif([0-9]+)\\.([0-9]+)"

matchG :: String -> String -> [String]
matchG s regex =
    let (_,_,_,grps) = s =~ regex :: (String,String,String,[String])
    in grps

getVmFromUuid uuid = TL.unpack . strObjectPath <$> comCitrixXenclientXenmgrFindVmByUuid xenmgrService xenmgrRootObj uuid
getVmFromDomid domid = TL.unpack . strObjectPath <$> comCitrixXenclientXenmgrFindVmByDomid xenmgrService xenmgrRootObj domid
getVmName vmObj = comCitrixXenclientXenmgrVmGetName xenmgrService vmObj
getVmDomid vmObj = comCitrixXenclientXenmgrVmGetDomid xenmgrService vmObj
getVmState vmObj = comCitrixXenclientXenmgrVmGetState xenmgrService vmObj
getVmNicMac vmObj devid = do debug $ printf "nicObj - %s " nicObj
                             comCitrixXenclientVmnicGetMacActual xenmgrService nicObj
                             where nicObj = vmObj ++ "/nic/" ++ devid
getVmFirewallRules vmObj = comCitrixXenclientXenmgrVmListNetFirewallRules xenmgrService vmObj

getVmNameFromUuid uuid = 
    if (uuid == domain0uuid)
       then return "dom0"
       else do vmObj <- (getVmFromUuid $ uuidStr uuid) `catchError` onErr
               if null vmObj
                  then return ""
                   else (getVmName vmObj) `catchError` onErr
    where onErr ex  = warn (show ex)  >> return ""
                  

xsVifMac :: String -> String -> IO (Maybe String)
xsVifMac gDomid gDevid = xsRead $ printf "/local/domain/%s/device/vif/%s/mac" gDomid gDevid

xsVifRemove :: String -> String -> IO ()
xsVifRemove gDomid gDevid = xsRm $ printf "/local/domain/%s/device/vif/%s" gDomid gDevid

xenopsDelVifCmd :: String -> String -> String -> String
xenopsDelVifCmd = printf "xenops del_vif -netty DriverDomain -domid %s -backend-domid %s -devid %s" 

xenopsAddVifCmd :: (String -> String -> String -> String -> String)
xenopsAddVifCmd =  printf "xenops add_vif -netty DriverDomain -domid %s -backend-domid %s -mac %s -devid %s"

xsBackendVifNode :: String -> String
xsBackendVifNode = printf "/local/domain/%s/backend/vif" 

xsVifNetwork :: String -> String -> String -> IO (Maybe String)
xsVifNetwork domid devid backendDomid = xsRead $ printf "%s/%s/%s/bridge" (xsBackendVifNode backendDomid)  domid devid
--xsVifMac domid devid backendDomid = xsRead $ printf "%s/%s/%s/mac" (xsBackendVifNode backendDomid) domid devid

xsVifState :: String -> String -> String -> IO (Maybe String)
xsVifState domid devid backendDomid = xsRead $ printf "%s/%s/%s/state" (xsBackendVifNode backendDomid)  domid devid

xsVifFrontend :: String -> String -> String -> IO (Maybe String)
xsVifFrontend domid devid backendDomid = xsRead $ printf "%s/%s/%s/frontend" (xsBackendVifNode backendDomid) domid devid

xsGuestDomains :: String -> IO ([String])
xsGuestDomains backendDomid = xsDir (xsBackendVifNode backendDomid)

xsDevIds :: String -> String -> IO ([String])
xsDevIds backendDomid guestDomid = xsDir $ printf "%s/%s" (xsBackendVifNode backendDomid) guestDomid

xsVifDisconnect :: String -> String -> String -> IO (Maybe String)
xsVifDisconnect domid devid backendDomid = xsVifFrontend domid devid backendDomid >>= f where
                                                      f Nothing = return Nothing
                                                      f (Just v) = xsRead (v++"/disconnect")

xsSetVifDisconnect :: String -> String -> String -> String -> IO ()
xsSetVifDisconnect domid devid backendDomid disconnect = xsVifFrontend domid devid backendDomid >>= f where
                                                                f Nothing = return ()
                                                                f (Just v)  = xsWrite (v ++ "/disconnect")  disconnect

anyM p = orM . map p
orM = foldr orM2 (return False)
orM2 a b = do x <- a
              if x then return True else b
 
appStateSlaveInfo = liftIO . readMVar . nwsInfo =<< getAppState

slaveDomids :: App [DomainId]
slaveDomids = do
    networkSlaves <- initializedSlaves
    if (M.null networkSlaves)
        then do 
            debug $ printf "No slave is initialised yet. So return empty list of networks"
            return [] 
        else do 
            return $ map networkBackendDomid (M.elems networkSlaves)

initializedSlaves :: App (M.Map Uuid NetworkSlaveInfo)
initializedSlaves = do
    knownSlaves <- appStateSlaveInfo
    let (_, matchedSlaves) = M.mapEither slaveInitialized knownSlaves
    return matchedSlaves
 
    where
        slaveInitialized slaveInfo = if (networkSlaveState slaveInfo == Initialized)
                                        then Right slaveInfo
                                        else Left slaveInfo      

getFileContents :: FilePath -> IO String
getFileContents filePath = fromMaybe "" <$> maybeGetContents filePath

objPathToStr :: ObjectPath -> String
objPathToStr = TL.unpack . strObjectPath

getSlaveInfo :: AppState -> (Maybe String) -> IO (Maybe NwsObjInfo)
getSlaveInfo appState Nothing = return Nothing
getSlaveInfo appState nwdNetwork = fmap (msum . map (M.lookup (fromJust nwdNetwork))) (mapM readMVar $ nwMvarList appState)  

getNetworks :: AppState -> IO [(M.Map String NwsObjInfo)]
getNetworks appState  = mapM readMVar $ nwMvarList appState
                                    
nwMvarList appState = map ($ appState) [bridgedNwObjs, sharedNwObjs, wirelessNwObjs, internalNwObjs, anyNwObjs]

nwLookup network appState =  do
     nwdNws <- getNetworks appState
     return $ M.lookup network $ M.unions nwdNws

backendLookup uuid appState = do
     backends <- readMVar $ nwsInfo appState 
     return $ M.lookup uuid backends

first (x:_) = Just x
first _ = Nothing

getConfigProperty :: String -> String -> Maybe String
getConfigProperty value property=
    first $ catMaybes $ map f $ split ',' value where
      f x = case split '=' x of
	[k,v] | k == property -> Just v
        _ -> Nothing

dbusObjsFromNwType nwType id
        | nwType == eNETWORK_TYPE_WIRED = map (dbusObjFromNwType id nwType) [eCONNECTION_TYPE_BRIDGED, eCONNECTION_TYPE_SHARED]
        | otherwise = (dbusObjFromNwType id nwType eCONNECTION_TYPE_UNKNOWN):[]

dbusObjFromNwType :: String -> String -> String -> String
dbusObjFromNwType nwId nwType connType = nwName nwId nwTypStr connTypeStr
    where 
      (nwTypStr, connTypeStr) = case () of
       _ | (nwType == eNETWORK_TYPE_ANY) -> (nwType, "")
         | (nwType == eNETWORK_TYPE_WIFI || nwType == eNETWORK_TYPE_MODEM) -> (nwType, "shared")
         | (nwType == eNETWORK_TYPE_INTERNAL) -> (nwType, "")
         | (nwType ==  eNETWORK_TYPE_WIRED) ->
                           if (connType == eCONNECTION_TYPE_SHARED)
                              then (nwType, "shared")
                              else (nwType, "bridged")
         | (nwType == eNETWORK_TYPE_VPN) -> ("vpn", "")
         | otherwise -> (nwType, connType)

      nwName nwId nwType "" = printf "/%s/%s" nwType nwId 
      nwName nwId nwType connType = printf "/%s/%s/%s" nwType nwId connType

-- Remove exported object paths
hideObj :: String -> Rpc ()
hideObj path =
    do rpcHide $ fromString path
       info $ "Removed object path " ++ path

getMatchingNetworkConfigs :: String -> String -> String  -> Rpc [(String, String)]
getMatchingNetworkConfigs nwType uuid mac = do
    nws <- configGetNetworksOfType nwType
    let nwIds = if (null mac)
                   then matchJustUuids nws
                   else let (nwsMacUuid, otherNws) = matchBoth nws
                        in  if (not $ M.null nwsMacUuid) 
                               then nwsMacUuid
                               else let nwsMac = matchJustMac otherNws
                                    in if (not $ M.null nwsMac)
                                          then nwsMac
                                          else matchJustUuids otherNws 

    debug $ printf "getNetworkConfig output %s" (show nwIds)
    return (M.toList nwIds)
    where
        matchJustUuids = M.filter (unmatchedValuesMaybe uuid "")  
        matchJustMac = M.filter (unmatchedValuesMaybe  "" mac)
        matchBoth = M.mapEither (unmatchedValuesEither uuid mac)
  
        unmatchedValuesEither uuid mac configStr = 
            let [uuidConf, macConf] = map (fromMaybe "") $ map (getConfigProperty configStr) [eNW_PROP_UUID, eNW_PROP_MAC]
            in case (uuid==uuidConf, mac==macConf) of
                    (True,True) -> Left configStr
                    other -> Right configStr

        unmatchedValuesMaybe uuid mac configStr = 
            let [uuidConf, macConf] = map (fromMaybe "") $ map (getConfigProperty configStr) [eNW_PROP_UUID, eNW_PROP_MAC]
            in (uuid==uuidConf && mac==macConf)

realMac "" = ""
realMac mac = 
     let (firstByte, remainingBytes) = splitAt 2 mac
	 newFirstByte = printf "%02X" ((read ("0x" ++ firstByte) :: Word8) .&. (complement 0x2))
     in newFirstByte ++ remainingBytes

virtualMac "" = ""
virtualMac mac = 
     let (firstByte, remainingBytes) = splitAt 2 mac
	 newFirstByte = printf "%02X" ((read ("0x" ++ firstByte) :: Word8) .|. 0x2)
     in newFirstByte ++ remainingBytes

networkTypes = [eNETWORK_TYPE_WIRED, eNETWORK_TYPE_WIFI, eNETWORK_TYPE_MODEM, eNETWORK_TYPE_ANY, eNETWORK_TYPE_INTERNAL, eNETWORK_TYPE_VPN]
networkConfig nwName = do
    case (nwName `matchG` "/([a-z]+)/([0-9]+)/*([a-z]*)") of
         [nwTypeL, nwId, connTypeL] -> do
              nwInfo <- configGetNetwork (nwTypeL ++ "/" ++ nwId)
              return (nwTypeL, nwId, connTypeL, nwInfo)
         otherwise -> return ("", "", "", "")

isKnownNetworkDomain knownSlaves uuid domid = do 
    case (M.lookup uuid knownSlaves) of
         Just slaveInfo -> if (domid == networkBackendDomid slaveInfo)
                              then return True
                              else return False
         other -> return False

checkNetworkDomainState state knownSlaves uuid domid = do
    case (M.lookup uuid knownSlaves) of
         Just slaveInfo -> if ((domid == networkBackendDomid slaveInfo) && (networkSlaveState slaveInfo == state))
                              then return True
                              else return False
         other -> return False

