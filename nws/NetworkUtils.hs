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

{-# LANGUAGE PatternGuards #-}
module NetworkUtils ( listNetworkInterfaces
                      , listPhyInterfaces
                      , isVifOrTunInterface
                      , isBridgeInterface
                      , isWirelessInterface
                      , isSerialInterface
                      , isPhyInterface
                      , disableReversePathFilter
                      , enableIpForwarding
                      , enableBridgeNetfilter
                      , disableBridgeNetfilter
                      , logAndExecuteIptables 
                      , memberOf
                      , bridgeExists
                      , joinBridge
                      , leaveBridge
                      , checkAndAddBridge 
                      , checkAndJoinBridge
                      , checkAndLeaveBridge
                      , removeBridge
                      , interfaceUp
                      , interfaceDown
                      , interfaceSubnet
                      , interfaceIndex
                      , interfaceMac
                      , interfaceType
                      , interfaceExists
                      , interfaceCarrier
                      , changeIfaceMac
                      , addIptableRulesIfMissing 
                      , isValidMac
                    ) where

import Data.Maybe
import Data.Char
import Data.Bits
import Data.Ord
import Data.List

import Text.Regex.Posix
import Text.Printf (printf)

import Directory

import System.Exit
import System.FilePath
import System.Posix.Files

import Control.Monad
import Control.Applicative
import Control.Monad.Error (catchError)

import Tools.Process
import Tools.Log
import Tools.Text
import Tools.Misc
import Tools.File

import Utils
import Error

data PciBusAddr = BDF Integer Integer Integer Integer

sysfsnet = "/sys/class/net"
sysfsnetContents = getDirectoryContents_nonDotted sysfsnet
-- network interface sysfs locations ordered by their pci bus address.
-- if address can't be worked out, iface gets put towards the end of the list.
sysfsnetOrderedContents =
  order =<< sysfsnetContents where
  order faces = map snd . sortBy (comparing fst) <$> weightedFaces
    where
      weightedFaces = mapM (\f -> (,) <$> weight f <*> pure f) faces

  weight face = f <$> ifacePciBusAddr face where
    f Nothing = 0xfffffffff
    f (Just (BDF a b c d)) =
              (a `shiftL` 20)
          .|. (b `shiftL` 12)
          .|. (c `shiftL` 4)
          .|.  d

-- try to work out pci bus address of network interface from its sysfs path
ifacePciBusAddr :: String -> IO (Maybe PciBusAddr)
ifacePciBusAddr face = do
    p <- (sysfsnet </>) <$> readSymbolicLink (sysfsnet </> face)
    scan (reverse $ splitPath p)
    where
      fromHex s = maybeRead ("0x" ++ s)
      scan :: [String] -> IO (Maybe PciBusAddr)
      scan [] = return Nothing
      scan ("../" : _) = return Nothing
      scan (iname : "net/" : addr : xs) = return $
          let addr' = reverse $ drop 1 (reverse addr) in
          case () of
            _ |   [a,b,x] <- split ':' addr'
                , [c,d]   <- split '.' x     ->
                  let h = fromHex in
                  BDF <$> h a <*> h b <*> h c <*> h d
              | otherwise -> Nothing
      scan (x : xs) = scan xs

listNetworkInterfaces = sysfsnetOrderedContents

listPhyInterfaces = catMaybes <$> (mapM isPhy =<< sysfsnetOrderedContents)
    where isPhy iface = isPhyInterface iface >>= f where f True = return (Just iface)
                                                         f False = return Nothing                      

isVifOrTunInterface :: FilePath -> IO (Bool)
isVifOrTunInterface iface = do
    devtype <- strip <$> getFileContents (sysfsnet </> iface </> "device" </> "devtype")
    return ((devtype =~ "vif" :: Bool) || (iface =~ "tap" :: Bool))

isBridgeInterface :: String -> IO (Bool)
isBridgeInterface iface =  doesDirectoryExist (sysfsnet </> iface </> "bridge")

memberOf :: String -> IO String
memberOf iface = do
    bridgeDetails <- getFileContents (sysfsnet </> iface </> "brport" </> "bridge" </> "uevent")
    return $ case bridgeDetails of 
                  "" -> "" 
                  other -> let grps = (other `matchG` "INTERFACE=(\\S+)$") in
                           case grps of
                                [x] -> x
                                other -> ""

isWirelessInterface :: String -> IO (Bool)
isWirelessInterface iface =  doesDirectoryExist (sysfsnet </> iface </> "wireless")

isSerialInterface :: String -> IO (Bool)
isSerialInterface iface = do
    devtype <- getFileContents  (sysfsnet </> iface </> "type")
    return $ case (devtype , (iface =~ "^(ppp)" :: Bool)) of
                  ("512", _) -> True
                  (_, True) -> True
                  otherwise -> False

isPhyInterface :: String -> IO Bool
isPhyInterface iface = do 
    devExists <- doesDirectoryExist(sysfsnet </> iface </> "device")
    isVif <- isVifOrTunInterface iface

    case (devExists, isVif) of
         (True, False) -> return True
         otherwise -> return False

disableReversePathFilter iface = spawnShell $ printf "echo 0 > /proc/sys/net/ipv4/conf/%s/rp_filter" iface
enableIpForwarding = spawnShell "echo 1 > /proc/sys/net/ipv4/ip_forward"

enableBridgeNetfilter = do 
    spawnShell "echo 1 > /proc/sys/net/bridge/bridge-nf-call-iptables"
    spawnShell "echo 1 > /proc/sys/net/bridge/bridge-nf-call-ip6tables"
    spawnShell "echo 1 > /proc/sys/net/bridge/bridge-nf-call-arptables"
    logAndExecuteIptables $ printf "-t nat -I POSTROUTING -m physdev --physdev-is-bridge -j ACCEPT"

disableBridgeNetfilter = void $ do 
    spawnShell "echo 0 > /proc/sys/net/bridge/bridge-nf-call-iptables"
    spawnShell "echo 0 > /proc/sys/net/bridge/bridge-nf-call-ip6tables"
    spawnShell "echo 0 > /proc/sys/net/bridge/bridge-nf-call-arptables"

logAndExecuteIptables cmd = void $ do
    debug cmd
    spawnShell $ "iptables " ++ cmd

bridgeExists :: String -> IO Bool
bridgeExists bridge = doesDirectoryExist (sysfsnet </> bridge </> "brif")

joinBridge bridge interface = unless (null bridge) $ void $ do
    debug $ printf "Joining %s to %s" interface bridge
    spawnShell $ printf "brctl setfd %s 0" bridge
    spawnShell $ printf "brctl addif %s %s" bridge interface
    spawnShell $ printf "ifconfig %s inet 0.0.0.0 promisc up" interface

leaveBridge bridge interface = unless (null bridge) $ void $ do 
    spawnShell $ printf "brctl delif %s %s" bridge interface
    spawnShell $ printf "ip link set %s down" interface

checkAndAddBridge :: String -> IO ()
checkAndAddBridge bridge = do
    exists <- bridgeExists bridge
    case exists of 
         False -> do debug $ "Adding bridge " ++ bridge
                     readProcessOrDie "brctl" ["addbr", bridge] []
                     return ()
         otherwise -> do debug $ bridge ++ "  already exists"
                         return ()
         
    readProcessOrDie "brctl" ["stp", bridge, "off"] []
    readProcessOrDie "brctl" ["setfd", bridge, "0"] []
    return ()

checkAndJoinBridge :: String -> String -> IO Bool
checkAndJoinBridge bridge interface = do
    curBridge <- memberOf interface
    case (null curBridge, curBridge == bridge) of
         (True,_) -> do joinBridge bridge interface
                        return True
         (_, True) -> do debug $ printf "%s is already added to %s" interface bridge
                         return True
         (_, False) -> do error $ printf "%s cannot be added to  %s when it is already added to %s"
                          return False

checkAndLeaveBridge :: String -> String -> IO Bool
checkAndLeaveBridge bridge interface = do
    curBridge <- memberOf interface
    if (curBridge == bridge)
       then do leaveBridge bridge interface
               return True
       else do error $ printf "%s is not on %s bridge" interface bridge
               return False

removeBridge :: String -> IO ()
removeBridge bridge = do
      exists <- doesDirectoryExist (sysfsnet </> bridge </> "brif")
      when exists $ do
         ifs <- getDirectoryContents (sysfsnet </> bridge </> "brif")
         mapM_ (delif bridge) ifs
         readProcessWithExitCode_closeFds "ifconfig" [bridge, "down"] []
         readProcessWithExitCode_closeFds "brctl" ["delbr", bridge] []
         return ()

delif bridge iface  = readProcessWithExitCode_closeFds "brctl" ["delif", bridge, iface] []


interfaceUp iface = readProcessWithExitCode_closeFds "ifconfig" [iface, "inet", "0.0.0.0", "up"] []

interfaceDown iface = readProcessWithExitCode_closeFds "ifconfig" [iface, "down"] []

interfaceSubnet iface = do out <- spawnShell $ printf "ip addr show %s | grep inet" iface
                           case (out `matchG` "([0-9]+\\.[0-9]+\\.[0-9]+)\\.[0-9]+") of
                                [prefix] -> return $ prefix ++ ".0"
                                _ -> return ""

interfaceIndex :: String -> IO String
interfaceIndex interface = strip <$> getFileContents (sysfsnet </> interface </> "ifindex")

interfaceMac :: String -> IO String
interfaceMac interface = strip <$> getFileContents (sysfsnet </> interface </> "address")

interfaceType :: String -> IO String
interfaceType interface = strip <$> getFileContents (sysfsnet </> interface </> "type")

interfaceExists :: String -> IO Bool
interfaceExists interface = doesDirectoryExist (sysfsnet </>  interface)

interfaceCarrier :: String -> IO Bool
interfaceCarrier interface = (== "1") <$> (strip <$> getFileContents carrierPath) `catchError` (\ex -> return "0")
    where
        carrierPath = sysfsnet </> interface </> "carrier"
                               

changeIfaceMac iface mac = do
    interfaceDown iface
    readProcessWithExitCode_closeFds "ifconfig" [iface, "hw", "ether", mac] []
    interfaceUp iface

addIptableRulesIfMissing :: [String] -> String -> IO ()
addIptableRulesIfMissing iptablesOut iptablesArgs = do
    unless (any (match iptablesArgs) iptablesOut) $ do
        (exitCode, _,err) <- do
                     debug $ "iptables " ++ iptablesArgs
                     readProcessWithExitCode_closeFds "iptables" (words iptablesArgs) []
        case exitCode of
             ExitSuccess -> return ()
             _ -> error $ printf "cannot add rule %s : %s" iptablesArgs err

    where match template rule  = rule =~ template :: Bool

isValidMac mac = ((map toUpper mac) =~ "^([0-9A-F]{2}[:-]){5}([0-9A-F]{2})$" :: Bool)
