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

import Test.Hspec
import Test.QuickCheck

import Data.Attoparsec.Ping
       (PingError(..), PingResult(..), pingResult)
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
  \Reply from 127.0.0.1: bytes=32 time<1ms TTL=128\n\
  \Reply from 127.0.0.1: bytes=32 time<1ms TTL=128\n\
  \Reply from 127.0.0.1: bytes=32 time<1ms TTL=128\n\
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

main :: IO ()
main =
  hspec $
  describe "Data.Attoparsec.Ping.pingResult" $ do
    it "returns PingError HostNotFound when host not found" $
      parseOnly pingResult hostNotFoundString `shouldBe`
      Right (PingError $ HostNotFound "unknown.host.com")
    it "returns PingResult when pinging ip" $
      parseOnly pingResult ping4TimesToIp `shouldBe`
      Right
        PingSucceed
        { hostAddress = read "127.0.0.1"
        , hostNameResolved = Nothing
        , bytesOfData = 32
        , lineResults = []
        }
    it "returns PingResult when pinging hostname" $
      parseOnly pingResult ping4TimesToIpWithHostName `shouldBe`
      Right
        PingSucceed
        { hostAddress = read "127.0.0.1"
        , hostNameResolved = Just "the.host.name"
        , bytesOfData = 32
        , lineResults = []
        }
    it "returns Fails when random string" $
      property $ \x -> parseOnly pingResult x `shouldSatisfy` isLeft
