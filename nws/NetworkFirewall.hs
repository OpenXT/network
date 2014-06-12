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
module NetworkFirewall (applyFirewallRules
                        , cleanupFirewallRules
                        , initBridgedFiltering 
                        , addForwardChain 
                        , configGetBridgeFiltering
                       ) where

import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map as M

import Text.Regex.Posix
import Text.Printf (printf)

import Directory

import System.FilePath.Posix
import System.Exit

import Control.Monad
import Control.Applicative

import Tools.Process
import Tools.Log
import Tools.Text
import Tools.Misc
import Tools.File
import Tools.XenStore

import App
import Utils
import Error
import NetworkUtils
import Rpc.Autogen.NetworkSlaveConst

domidToUuid :: String -> IO (Maybe String)
domidToUuid domid =
   -- can's use getDomainUuid stuff here as we can't read stubdom vm objs from xenstore
   if (domid == "0")
      then return $ Just "00000000-0000-0000-0000-000000000000"
      else do
         uuids <-  xsDir "/xenmgr/vms"
         (listToMaybe.catMaybes) <$> (mapM (matchDomid domid) uuids)

    where
        xsVmDomid uuid = printf "/xenmgr/vms/%s/domid" uuid
        xsVmStubdomid uuid = printf "/xenmgr/vms/%s/stubdomid" uuid
   
        xsDomid uuid = xsRead $ xsVmDomid uuid
        xsStubdomid uuid = xsRead $ xsVmStubdomid uuid

        matchDomid :: String -> String -> IO (Maybe String)
        matchDomid domid uuid = do
           domidM <- fromMaybe "" <$> xsDomid uuid
           stubdomidM <- fromMaybe "" <$> xsStubdomid uuid
           case () of
                _ | (domidM == domid) -> return $ Just uuid
                  | (stubdomidM == domid) -> return $ Just uuid
                  | otherwise -> return Nothing

addForwardChain :: String -> String -> String -> IO ()
addForwardChain inputIf outputIf interface = do 
    (_, out, _) <- readProcessWithExitCode_closeFds "/usr/sbin/iptables" ["-S", "FORWARD"] []
    let output = lines out
    configGetBridgeFiltering >>= \x -> when (x && outputIf /= interface) $ 
         addIptableRulesIfMissing output (printf "-A FORWARD -i %s -o %s -m physdev --physdev-in %s -j FORWARD_%s" outputIf outputIf interface outputIf)
    mapM_ (addIptableRulesIfMissing output) filters
    where
        filters =  [ printf "-A FORWARD -i %s -o %s -j ACCEPT" inputIf outputIf
                   , printf "-A FORWARD -i %s -o %s -j ACCEPT" outputIf inputIf
                   , printf "-A FORWARD -i %s -o %s -j ACCEPT" inputIf inputIf
                   , printf "-A FORWARD -i %s -j REJECT" inputIf
                   , printf "-A FORWARD -o %s -j REJECT" inputIf
                   ]


cleanupFirewallRules vif bridgeI = configGetBridgeFiltering >>= \x -> when (x) $ do
    bridge <- if (null bridgeI) 
                 then do
                    out <- words <$> (spawnShell $ printf "iptables -L FORWARD -v | grep %s | awk '{ print $6 }'" vif)
                    if (null out) then return "" else return $ head out
                 else return bridgeI

    cleanupVifInRules bridge
    cleanupVifOutRules bridge
    removeChain vifChainOut
    removeChain vifChainIn

    where
        vifIfaceIn="--physdev-in " ++ vif
        vifIfaceOut="--physdev-is-bridged --physdev-out " ++ vif

        vifChain="FORWARD_" ++ vif
        vifChainIn="FORWARD_" ++ vif ++ "_IN"
        vifChainOut="FORWARD_" ++ vif ++ "_OUT"

        cleanupVifOutRules bridge = do
            let bridgeIn = "-i " ++ bridge
                bridgeOut="-o " ++ bridge
            unless (null bridge) $ logAndExecuteIptables $ printf "-D FORWARD %s -m physdev %s -j %s" bridgeIn vifIfaceIn vifChainOut 

        cleanupVifInRules bridge = do
            let bridgeOut="-o " ++ bridge
                bridgeChain="FORWARD_" ++ bridge
            unless (null bridge) $ logAndExecuteIptables $ printf "-D %s %s -m physdev %s -j %s" bridgeChain bridgeOut vifIfaceOut vifChainIn
        
domAndDevIdFromVif vif = vif `matchG` "vif([0-9]+)\\.([0-9]+)"

applyFirewallRules vif bridge = void $ configGetBridgeFiltering >>= \x -> when x $ do
    let [domid, devid] = domAndDevIdFromVif vif
   
    uuidM <- domidToUuid domid
    unless (uuidM == Nothing) $ do
        let uuid = fromJust uuidM
        when (bridge =~ "brbridged" :: Bool) $ do -- hack to detect it's a bridged network

            initChain vifChainIn
            initChain vifChainOut
        
            -- add packet destined to vif must be checked by vif input filters i.e, frame that'll leave the bridge and vif port on the bridge should go through
            -- input filtering rules for that vif. These rules will not get to run when the vif is on a shared/NAT'ed network as you can't filter the packets
            -- based on phys-out interface
            insertRuleIfMissing bridgeChain (printf "%s -m physdev %s -j %s" bridgeOut vifIfaceOut vifChainIn)
         
            -- any packet coming in from vif and out bridge must first be checked by vif chain out 
            insertRuleIfMissing "FORWARD" (printf "%s -m physdev %s -j %s" bridgeIn vifIfaceIn vifChainOut)
    
            -- now that the framework is setup, process the firewall rules defined by the user
            (exitCode, out, _) <- readProcessWithExitCode_closeFds "db-nodes-dom0"  [(fwConfRoot uuid)] []
        
            mapM_ (processRule uuid) (sort $ words out) -- sort the rule indices before applying them

        return ()
        
    where
        bridgeIn = "-i " ++ bridge
        bridgeOut = "-o " ++ bridge
        bridgeChain = "FORWARD_" ++ bridge

        vifIfaceIn = "--physdev-in " ++ vif
        vifIfaceOut = "--physdev-is-bridged --physdev-out " ++ vif

        vifChain = "FORWARD_" ++ vif
        vifChainIn = "FORWARD_" ++ vif ++ "_IN"
        vifChainOut = "FORWARD_" ++ vif ++ "_OUT"
        fwConfRoot uuid = "/firewall-rules/" ++ uuid

        processRule uuid index = void $ do
            direction <- (map toLower) <$> strip <$> (dbReadDom0 (printf "%s/%s/direction" (fwConfRoot uuid) index))
            remoteIp <- strip <$> dbReadDom0 (printf "%s/%s/remote-ip" (fwConfRoot uuid) index)
            extra <- strip <$> dbReadDom0 (printf "%s/%s/extra" (fwConfRoot uuid) index)

            unless (null direction || null remoteIp) $ void $ do
                case () of
                     _ | (direction == "in") -> logAndExecuteIptables $ printf "-I %s %s --source %s -j DROP" vifChainIn extra remoteIp
                       | (direction == "out") -> logAndExecuteIptables $ printf "-I %s %s --destination %s -j DROP" vifChainOut extra remoteIp
                       | otherwise -> debug $ printf "unexpected direction - '%s'" direction

        dbNodesDom0 path = do
            (exitCode, out, _) <- readProcessWithExitCode_closeFds "db-nodes-dom0" [path] []
            return out

        dbReadDom0 path = do
            (exitCode, out, _) <- readProcessWithExitCode_closeFds "db-read-dom0" [path] []
            return out

appendRuleIfMissing :: String -> String -> IO ()
appendRuleIfMissing table ruleArgs = do
    (_, out, _) <- readProcessWithExitCode_closeFds "/usr/sbin/iptables" ["-S", table] []
    addIptableRulesIfMissing (lines out) rule
    where rule = printf " -A %s %s" table ruleArgs

insertRuleIfMissing :: String -> String -> IO ()
insertRuleIfMissing table ruleArgs = do
    (_, out, _) <- readProcessWithExitCode_closeFds "/usr/sbin/iptables" ["-S", table] []
    addIptableRulesIfMissing (lines out) rule
    where rule = printf " -I %s %s" table ruleArgs

removeChain chain = do
    logAndExecuteIptables $ "-F " ++ chain
    logAndExecuteIptables $ "-X " ++ chain
   
initChain chain = do
    logAndExecuteIptables $ "-N " ++ chain
    logAndExecuteIptables $ "-F " ++ chain

initBridgedFiltering bridge interface = configGetBridgeFiltering >>= \x -> when x $ do
    let bridgeChain = "FORWARD_" ++ bridge
    initChain bridgeChain
    logAndExecuteIptables $ printf "-I %s -o %s -m physdev --physdev-is-bridged --physdev-out %s -j ACCEPT" bridgeChain bridge interface

configGetBridgeFiltering :: IO (Bool)
configGetBridgeFiltering = do
    (_, enableFiltering,_) <- readProcessWithExitCode_closeFds "db-read-dom0" [path] []
    if ((strip enableFiltering) == "true")
       then return True
       else return False
    where path = "/" ++ eCONFIG_BRIDGE_FILTERING
