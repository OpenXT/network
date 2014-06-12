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

{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils where

import Data.Int
import Data.String
import Text.Printf
import Text.Regex.Posix
import qualified Data.Text as T
import Data.Maybe
import Tools.XenStore
import Tools.File
import Rpc

import Control.Applicative

type DomainId = Int32
type DBusId = String
type Uuid = String
type XSPath = String

type NotifyHandler = DBusId -> DomainId -> Uuid -> [Variant] -> Rpc ()

-- Slave service and root object path
nwsService = "com.citrix.xenclient.networkslave"

nwsRootObj = "/"

domain0uuid :: Uuid
domain0uuid = fromString "00000000-0000-0000-0000-000000000000"

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

getFileContents :: FilePath -> IO String
getFileContents filePath = fromMaybe "" <$> maybeGetContents filePath

matchG :: String -> String -> [String]
matchG s regex =
    let (_,_,_,grps) = s =~ regex :: (String,String,String,[String])
    in grps

anyM p = orM . map p
orM = foldr orM2 (return False)
orM2 a b = do x <- a
              if x then return True else b
