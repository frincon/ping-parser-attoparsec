-- Copyright 2017 Fernando Rincon Martin
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Parser for ping output. Common Types.
-}
module Data.Attoparsec.Ping
  ( PingResult(..)
  , PingError(..)
  , LineResult(..)
  , ReplyResult(..)
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.IP
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Development.Placeholders

data PingResult
  = PingSucceed { hostAddress :: IPv4
                , hostNameResolved :: Maybe Text
                , bytesOfData :: Int
                , lineResults :: [LineResult] }
  | PingError PingError
  deriving (Eq, Show)

data PingError =
  HostNotFound Text
  deriving (Eq, Show)

data LineResult
  = ReplyFrom IPv4
              ReplyResult
  | RequestTimedOut
  | GeneralFailure
  deriving (Eq, Show)

data ReplyResult
  = ResponseReceived { bytes :: Int
                     , timeInMs :: Int
                     , ttl :: Int }
  | DestinationHostUnreachable
  deriving (Eq, Show)
