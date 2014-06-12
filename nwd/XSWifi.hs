--
-- Copyright (c) 2012 Citrix Systems, Inc.
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
module XSWifi where

import System
import System.IO
import System.Posix.Syslog

import Rpc
import Rpc.Core
import App
import Error
import Tools.Log
import Tools.XenStore
import Tools.Process
import Tools.Misc

import Data.Int
import Data.Word
import Data.String
import Data.Bits
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL

import Text.Printf
import Text.Regex.Posix

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

import Rpc.Autogen.NetworkClient as NC
import NwTypes
import NetworkSlaveMethods

data WifiAuth = Wep | NoAuth
     deriving (Show)
data WifiAp   = WifiAp {
      apAuth       :: WifiAuth
    , apSsid       :: [Word8]
    , apEssid      :: String
    , apMac        :: String
    , apFreq       :: Int
    , apMaxBitrate :: Int
    , apQuality    :: Int
    } deriving (Show)

-- security capabilities
nm_802_11_AP_SEC_NONE = 0x0
nm_802_11_AP_SEC_PAIR_WEP40 = 0x1
nm_802_11_AP_SEC_PAIR_WEP104 = 0x2
nm_802_11_AP_SEC_PAIR_TKIP = 0x4
nm_802_11_AP_SEC_PAIR_CCMP = 0x8
nm_802_11_AP_SEC_GROUP_WEP40 = 0x10
nm_802_11_AP_SEC_GROUP_WEP104 = 0x20
nm_802_11_AP_SEC_GROUP_TKIP = 0x40
nm_802_11_AP_SEC_GROUP_CCMP = 0x80
nm_802_11_AP_SEC_KEY_MGMT_PSK = 0x100
nm_802_11_AP_SEC_KEY_MGMT_802_1X = 0x200

-- Query first, then export
wifiXsQueryAndExport :: DomainId -> String -> DomainId -> Rpc ()
wifiXsQueryAndExport slaveDomid slaveObj guestDomid= do -- info "exporting wifi information to xenstore"
    wifiQueryNm slaveDomid slaveObj >>= f 
        where 
            f Nothing     = liftIO $ do 
                               debug $ printf "Remove xenstore wlan nodes for domain %d" guestDomid
                               xsRm (activeApPath guestDomid)
                               xsRm (fakeInfoApPath guestDomid)
            f (Just info) = do
                               debug $ printf "Export AP information %s " (activeApPath guestDomid)
                               wifiXsExportAp (activeApPath guestDomid) info
                               wifiXsExportFakeInfoAp guestDomid

            activeApPath domid = "/local/domain/" ++ show guestDomid ++ "/wlan/0"
            fakeInfoApPath domid = "/local/domain/" ++ show guestDomid ++ "/wlan/1"

wifiQueryNm domid slaveNw =  do
    apInfo <- withNetworkSlave domid (NC.comCitrixXenclientNetworkConfigGetExtraInfo slaveService slaveNw)
    if (M.null apInfo)
       then return Nothing
       else do 
               let wifiAp = WifiAp {apAuth = authMode apInfo
                             , apSsid = B.unpack $ BC.pack $ M.findWithDefault "" eACTIVE_AP_SSID apInfo
                             , apEssid = "XenClient Wireless"
                             , apMac  = M.findWithDefault "" eACTIVE_AP_HWADDRESS apInfo
                             , apFreq = (read (M.findWithDefault "246200" eACTIVE_AP_FREQUENCY apInfo) :: Int)  * 1000
                             , apMaxBitrate = read (M.findWithDefault "0" eACTIVE_AP_MAXBITRATE apInfo) :: Int
                             , apQuality = read (M.findWithDefault "100" eACTIVE_AP_STRENGTH apInfo) :: Int }
               return $ Just wifiAp
    where
        authMode apInfo
            | (wpa .&. wepMask) /= 0   = Wep
            | (rsn .&. wepMask) /= 0   = Wep
            | otherwise                = NoAuth
            where wpa = read (M.findWithDefault "0" eACTIVE_AP_WPAFLAGS apInfo) :: Word32
                  rsn = read (M.findWithDefault "0" eACTIVE_AP_RSNFLAGS apInfo) :: Word32
        wepMask =
            nm_802_11_AP_SEC_PAIR_WEP40  .|.
            nm_802_11_AP_SEC_PAIR_WEP104 .|.
            nm_802_11_AP_SEC_GROUP_WEP40 .|.
            nm_802_11_AP_SEC_GROUP_WEP104

-- Write ap information to XenStore
wifiXsExportAp :: String -> WifiAp -> Rpc ()
wifiXsExportAp path ap = do
    liftIO $ do
      -- Write SSID in hex format
      xsWrite (path ++ "/" ++ "ssid") hexSsid
      -- Write network name
      xsWrite (path ++ "/" ++ "essid") (apEssid ap)
      -- Mac, frequency, bitrate, quality, security
      xsWrite (path ++ "/" ++ "mac") (apMac ap)
      xsWrite (path ++ "/" ++ "frequency") (show $ apFreq ap)
      xsWrite (path ++ "/" ++ "quality") (show $ apQuality ap)
      xsWrite (path ++ "/" ++ "auth") (strAuth $ apAuth ap)
  where
    hexSsid        = concat $ map hex (apSsid ap)
    hex b          = printf "%02X" (fromIntegral b :: Int)
    strAuth NoAuth = "none"
    strAuth Wep    = "wep"

wifiXsExportFakeGsmAp :: DomainId -> Rpc ()
wifiXsExportFakeGsmAp domid =
    wifiXsExportAp (activeApPath domid) ap
  where
    activeApPath domid = "/local/domain/" ++ (show domid) ++ "/wlan/0"
    ap = WifiAp { apAuth = NoAuth
                , apSsid = [0x6A,0x65,0x64,0x74,0x65,0x73,0x74,0x36]
                , apEssid = "XenClient Wireless"
                , apMac  = "02:DE:1A:AD:BE:EF"
                , apFreq = 2462000
                , apMaxBitrate = 0
                , apQuality = 100 }

wifiXsExportFakeInfoAp :: DomainId -> Rpc ()
wifiXsExportFakeInfoAp domid =
        wifiXsExportAp (activeApPath domid) ap
  where
    activeApPath domid = "/local/domain/" ++ (show domid) ++ "/wlan/1"
    ap = WifiAp { apAuth = NoAuth
                , apSsid = [0x55, 0x73, 0x65, 0x20, 0x55, 0x49, 0x20, 0x56]
                , apEssid = essid
                , apMac  = "02:DE:1B:AD:BE:EF"
                , apFreq = 2462000
                , apMaxBitrate = 0
                , apQuality = 100 }
    essid = "Hit Ctrl+0 to change network"
