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
{-# LANGUAGE OverloadedStrings #-}

module Spec.Win32
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Data.Attoparsec.Ping.Win32
import Data.Attoparsec.Text (parseOnly)
import Data.Either (isLeft)
import Data.Text.Arbitrary

hostNotFoundString =
  "Ping request could not find host unknown.host.com. \
  \Please check the name and try again.\n\
  \"

ping4TimesToIp =
  "\n\
  \Pinging 127.0.0.1 with 32 bytes of data:\n\
  \Request timed out.\n\
  \Reply from 127.0.0.1: Destination host unreachable.\n\
  \Reply from 127.0.0.1: bytes=32 time=121ms TTL=128\n\
  \Reply from 127.0.0.1: bytes=32 time<1ms TTL=128\n\
  \\n\
  \Ping statistics for 127.0.0.1:\n\
  \    Packets: Sent = 4, Received = 4, Lost = 0 (0% loss),\n\
  \Approximate round trip times in milli-seconds:\n\
  \    Minimum = 0ms, Maximum = 0ms, Average = 0ms\n\
  \"

ping4TimesToIpWithHostName =
  "\n\
  \Pinging the.host.name [127.0.0.1] with 32 bytes of data:\n\
  \Request timed out.\n\
  \Reply from 127.0.0.1: Destination host unreachable.\n\
  \Reply from 127.0.0.1: bytes=32 time=121ms TTL=128\n\
  \Reply from 127.0.0.1: bytes=32 time<1ms TTL=128\n\
  \\n\
  \Ping statistics for 127.0.0.1:\n\
  \    Packets: Sent = 4, Received = 4, Lost = 0 (0% loss),\n\
  \Approximate round trip times in milli-seconds:\n\
  \    Minimum = 0ms, Maximum = 0ms, Average = 0ms\n\
  \"

expectedLines :: [LineResult]
expectedLines =
  [ RequestTimedOut
  , ReplyFrom (read "127.0.0.1") DestinationHostUnreachable
  , ReplyFrom
      (read "127.0.0.1")
      ResponseReceived {bytes = 32, timeInMs = 121, ttl = 128}
  , ReplyFrom
      (read "127.0.0.1")
      ResponseReceived {bytes = 32, timeInMs = 0, ttl = 128}
  ]

spec :: Spec
spec =
  describe "Data.Attoparsec.Ping.Win32.pingResult" $ do
    it "returns PingError HostNotFound when host not found" $
      parseOnly pingParser hostNotFoundString `shouldBe`
      Right (PingError $ HostNotFound "unknown.host.com")
    it "returns PingResult when pinging ip" $
      parseOnly pingParser ping4TimesToIp `shouldBe`
      Right
        PingSucceed
        { hostAddress = read "127.0.0.1"
        , hostNameResolved = Nothing
        , bytesOfData = 32
        , lineResults = expectedLines
        }
    it "returns PingResult when pinging hostname" $
      parseOnly pingParser ping4TimesToIpWithHostName `shouldBe`
      Right
        PingSucceed
        { hostAddress = read "127.0.0.1"
        , hostNameResolved = Just "the.host.name"
        , bytesOfData = 32
        , lineResults = expectedLines
        }
    it "returns Fails when random string" $
      property $ \x -> parseOnly pingParser x `shouldSatisfy` isLeft
