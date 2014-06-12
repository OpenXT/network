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

module Error
    ( NwsError
    , LocalErr (..)
    , localE
    ) where

import Rpc.Core

-- Placeholder
--type LocalErr = ()
data LocalErr = InternalError String
    deriving (Eq,Show)

data NwsError = NwsError {
      remoteErr :: Maybe RemoteErr
    , localErr  :: Maybe LocalErr }

instance IsRemoteError NwsError where
    fromRemoteErr call remote_err = NwsError (Just remote_err) Nothing
    toRemoteErr e                 = remoteErr e

instance Show NwsError where
    show (NwsError (Just remote) Nothing) = show remote
    show (NwsError Nothing (Just local))  = "local:" ++ message local
    show _ = "?"

localE :: LocalErr -> NwsError
localE e = NwsError Nothing (Just e)

message :: LocalErr -> String
message (InternalError m) = "internal error: " ++ m

code :: LocalErr -> Int
code (InternalError m) = 301
