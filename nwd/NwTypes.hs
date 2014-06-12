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

{-# LANGUAGE TypeSynonymInstances #-}
module NwTypes where

import Data.Int
import Data.String
import Text.Printf
import qualified Data.Text as T
import Rpc

type DomainId = Int
type DBusId = String
type XSPath = String

newtype Uuid = Uuid String deriving (Eq,Ord)

instance Show Uuid where
    show (Uuid s) = s

instance IsString Uuid where
    fromString str = Uuid str

--
-- Make Uuid instance of Variable for easier marshalling
--
instance Variable Uuid where
    toVariant uuid =
        toVariant (show uuid)

    fromVariant v =
      fmap Uuid $ fromVariant v

uuidStr :: Uuid -> String
uuidStr = show

uuidStrUnderscore :: Uuid -> String
uuidStrUnderscore uuid =
    map subst $ show uuid
  where
    subst '-' = '_'
    subst ch  = ch

emptyUuid :: Uuid
emptyUuid = fromString ""

domain0uuid :: Uuid
domain0uuid = fromString "00000000-0000-0000-0000-000000000000"

-- DBus signal handler
type NotifyHandler = DBusId -> DomainId -> Uuid -> ObjectPath -> [Variant] -> Rpc ()

-- Daemon service and root object path
nwdService = "com.citrix.xenclient.networkdaemon"
nwdRootObj = "/"

-- Slave service and root object path
slaveService = "com.citrix.xenclient.networkslave"
slaveRootObj = "/"


