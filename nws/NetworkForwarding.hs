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

module NetworkForwarding (initRtTables
                        , initUnreachRoutingTable
                        , initNATRules
                        , cleanupNATRules
                        , addNATMasqueradeRule 
                        , addRoutingTableEntry
                        , addRoutingTableForInterface 
                        , addRouteLookup 
                        , routeLookupExists 
                        , routingTableForInterfaceExists 
                        , dnsMasqPid
                        , startDNSMasqServer
                        , startDNSMasq
                        , stopDNSMasq
                        , stopAllDNSMasq 
                       ) where

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

import Utils
import Error
import NetworkUtils

rtTables = "/etc/iproute2/rt_tables"

initRtTables = do 
    addRoutingTableEntry "local" "255"
    addRoutingTableEntry "main" "254"
    addRoutingTableEntry "default" "253"
    addRoutingTableEntry "unspec" "0"
    addRoutingTableEntry "unreach" "100"

initUnreachRoutingTable :: IO ()
initUnreachRoutingTable = do 
    spawnShell "ip route add unreachable 0.0.0.0/0 table unreach"
    spawnShell "ip route flush cache" 
    return ()

initNATRules :: IO ()
initNATRules = do
    cleanupNATRules
    mapM natRuleAdd ["brbridged", "wlan", "ppp"]
    return ()

    where 
        natRuleAdd prefix = spawnShell $ printf "iptables -t nat -A POSTROUTING -o %s+ -j MASQUERADE" prefix

cleanupNATRules :: IO ()
cleanupNATRules = void $ spawnShell "iptables -t nat -F POSTROUTING"

addNATMasqueradeRule :: String -> IO ()
addNATMasqueradeRule outputIf = do
    (_, out, _) <- readProcessWithExitCode_closeFds "/usr/sbin/iptables" ["-t", "nat", "-S"] []
    addIptableRulesIfMissing (lines out) masqRule
    where
        masqRule = printf "-t nat -A POSTROUTING -o %s+ -j MASQUERADE" outputIf

routingTableExists :: String -> String -> IO Bool
routingTableExists rtName rtIndex = do 
    out <- spawnShell $ printf "grep -E '%s|%s' %s" rtIndex rtName rtTables
    debug $ printf "Add entry if missing - %s %s" rtIndex rtName
    rtExists <- mapM (removeEntry rtIndex rtName) $ lines out
    
    if or rtExists 
       then return True
       else return False

    where 
        removeRTEntry :: String -> String -> IO String
        removeRTEntry index name = spawnShell $ printf "sed -i '/^%s\\s\\+%s/d' %s" index name rtTables

        removeEntry :: String -> String -> String -> IO Bool
        removeEntry index name entry  = do 
            case (entry `matchG` "([0-9]+)\\s+(.+)") of 
                 [entryIndex, entryName] -> case (entryIndex==index, entryName==name) of
                                                 (True, True)  -> return True
                                                 (False, False)   -> return False
                                                 _ -> do debug $ printf "Remove RT entry %s %s" entryIndex entryName
                                                         removeRTEntry entryIndex entryName
                                                         return False
                 otherwise -> do let output = entry `matchG` "([0-9]+) +(.+)" 
                                 debug $ printf "Error when adding %s %s This shouldn't happen - %s" index name (show output)
                                 return False

-- add an entry in /etc/iproute2/rt_tables
addRoutingTableEntry :: String -> String -> IO ()
addRoutingTableEntry rtName rtIndex = do
    entryExists <- routingTableExists rtName rtIndex
    if entryExists
        then debug $ printf "Routing table %s already exists" rtName
        else do addEntry rtIndex rtName
                return ()
    where 
        addEntry :: String -> String -> IO String
        addEntry index name = do debug $ printf "Adding rt_tables entry %s %s" index name
                                 spawnShell $ printf "echo %s %s >> %s" index name rtTables

routingTableIndex :: String -> Int
routingTableIndex ifindex = 30 + read ifindex

routingTableForInterfaceExists :: String -> String -> IO Bool
routingTableForInterfaceExists outputIf ifindex = 
    routingTableExists rtName rtIndex
    where
        rtName = "rt-" ++ outputIf
        rtIndex = show (routingTableIndex ifindex)

addRoutingTableForInterface :: String -> String -> IO ()
addRoutingTableForInterface outputIf ifindex = do
    debug $ printf "Add routing table entry %s (%s)" rtName rtIndex
    addRoutingTableEntry rtName rtIndex

    where
        rtName = "rt-" ++ outputIf
        rtIndex = show (routingTableIndex ifindex)

addRouteLookup :: String -> String -> String -> IO ()
addRouteLookup ifIndex rtTable inputIf = do
    rtExists <- routeLookupExists rtTable inputIf
    case rtExists of
       [True, True]   -> debug $ "Route lookup rules exist for " ++ inputIf
       [True, False]  -> addRouteLookupRule unreachPref "unreach" inputIf
       [False, True]  -> addRouteLookupRule routePref rtTable inputIf 
       [False, False] -> do addRouteLookupRule routePref rtTable inputIf
                            addRouteLookupRule unreachPref "unreach" inputIf
    where
        index = read ifIndex :: Int
        routePref = 100 + index
        unreachPref = 200 + index
        
        addRouteLookupRule pref table iif = do
          readProcessOrDie "ip" ["rule", "add", "pref", show pref
                                , "iif", iif, "table", table] []
          return ()

-- check if ip rule for route lookup exists
routeLookupExists :: String -> String -> IO [Bool]
routeLookupExists rtname inputIf = do
    (_, rules, _) <- readProcessWithExitCode_closeFds "/bin/ip"  ["rule", "show"] []
    let lookupTables = map (parseLookup inputIf) $ lines rules

    debug $ "lookup tables from ip rule show " ++ show lookupTables

    return  [rtname `elem` lookupTables, "unreach" `elem` lookupTables]
    where
        parseLookup interface rule = do
            case rule `matchG` "iif\\s+(\\S+)\\s+lookup\\s+(\\S+)" of
                 [iif, lookupTable] -> case (iif == interface) of
                                            True -> lookupTable
                                            False -> ""
                 otherwise -> ""

dnsMasqConfig = printf "/etc/dnsmasq-config/dnsmasq.%s"
dnsMasqScript :: String -> String -- monomorphism
dnsMasqScript = printf "/etc/dnsmasq-config/dnsmasq_script.%s"
dnsMasqPidFile = printf "/var/run/dnsmasq-%s.pid"

dnsMasqPid inputIf = getFileContents $ dnsMasqPidFile inputIf

startDNSMasqServer :: String -> (Maybe String) -> String -> String -> String -> IO ()
startDNSMasqServer inputIf outputIf inputIfIP dhcpRangeStart dhcpRangeEnd = do
    generateConfig inputIf outputIf inputIfIP dhcpRangeStart dhcpRangeEnd
    generateScript inputIf
    restartDNSMasqProcess inputIf

    where
        dnsMasqTemplate = "/etc/network-daemon/dnsmasq-template"
        dnsMasqScriptTemplate = "/etc/network-daemon/dnsmasq-script-template"
        resolvConfPath = "/var/volatile/etc"

        sedReplaceStr = "sed -e 's/<IFACE>/%s/g' " ++ 
                            "-e 's/<BRSHARED_IP>/%s/g' " ++
                            "-e 's/<DHCP_RANGE_START>/%s/g' " ++
                            "-e 's/<DHCP_RANGE_END>/%s/g' " ++
                            "-e 's/<RESOLV_CONF>/%s/g' " ++
                            "-e 's|<RESOLV_CONF_PATH>|%s|g' "

        generateConfig inIf outIf inIfIP dhcpStart dhcpEnd = do
            case outIf of
                Just outputIf -> do
                    let resolvConf = "resolv.conf." ++ outputIf
                        sedStr = sedReplaceStr ++ "-e 's/<DNSOUT_IFACE>/%s/g' "
                        sedCmd = sedStr ++ " %s > %s"
                    safeSpawnShell $ printf sedCmd inIf inIfIP dhcpStart dhcpEnd resolvConf resolvConfPath outputIf dnsMasqTemplate (dnsMasqConfig inIf)
                Nothing -> do
                    let resolvConf = "resolv.conf"
                        sedStr = sedReplaceStr ++ "-e '/<DNSOUT_IFACE>/d' "
                        sedCmd = sedStr ++ " %s > %s"
                    safeSpawnShell $ printf sedCmd inIf inIfIP dhcpStart dhcpEnd resolvConf resolvConfPath dnsMasqTemplate (dnsMasqConfig inIf)
        generateScript inIf = do
            safeSpawnShell $ printf "sed -e 's/<DOMSTORE_KEY>/%s/g' '%s' > '%s'" inIf dnsMasqScriptTemplate (dnsMasqScript inIf)
            safeSpawnShell $ printf "chmod +x '%s'" (dnsMasqScript inIf)

        restartDNSMasqProcess inputIf = do stopDNSMasq inputIf
                                           startDNSMasq inputIf

startDNSMasq inputIf = do 
    (exitCode,_,err) <- readProcessWithExitCode_closeFds "dnsmasq" ["-C", (dnsMasqConfig inputIf)] []
    case exitCode of 
         ExitSuccess -> debug $ printf "Started dnsmasq for %s" inputIf
         other -> error $ printf "Failed to start dnsmasq for %s with error : %s" inputIf err
                                                   
stopDNSMasq inputIf = dnsMasqPid inputIf >>= \pid ->
    void $ unless (null pid) $ void $ do
        debug $ printf "Stop dnsmasq (%s) for %s" (pid) inputIf
        spawnShell $ "kill -s SIGINT " ++ pid

stopAllDNSMasq =  do 
    pidFiles <- spawnShell $ printf "ls /var/run/dnsmasq*.pid" 
    mapM_ (stopProcessWithPid) $ lines pidFiles
    where 
        stopProcessWithPid pidFile = getFileContents pidFile >>= \pid -> unless (null pid) $ void $ spawnShell $ "kill " ++ pid
