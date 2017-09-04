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

module Data.Attoparsec.Ping
  ( pingResult
  , PingResult(..)
  , PingError(..)
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

pingResult :: Parser PingResult
pingResult = parseHostNotFound <|> parseSucceed <|> fail "Not recognized input"

parseHostNotFound :: Parser PingResult
parseHostNotFound = do
  string "Ping request could not find host "
  hostName <- manyTill anyChar (string ". ")
  string "Please check the name and try again."
  return $ PingError $ HostNotFound (T.pack hostName)

parseSucceed :: Parser PingResult
parseSucceed = do
  endOfLine
  string "Pinging "
  (maybeHostName, hostAddress) <- parseHostNameAndAddress
  bytesOfData <- decimal
  string " bytes of data:"
  endOfLine
  lineResults <- parseLineResults
  return
    PingSucceed
    { hostAddress = hostAddress
    , hostNameResolved = maybeHostName
    , bytesOfData = bytesOfData
    , lineResults = lineResults
    }

parseHostNameAndAddress :: Parser (Maybe Text, IPv4)
parseHostNameAndAddress = do
  hostNameAndOrAddress <- T.pack <$> manyTill anyChar (string " with ")
  case T.words hostNameAndOrAddress of
    [address] -> return (Nothing, parseIp4 address)
    [hostName, addressWithBrackets] ->
      return (Just hostName, parseIp4WithBrackets addressWithBrackets)
  where
    parseIp4 address = read (T.unpack address)
    parseIp4WithBrackets addressWithBrackets =
      parseIp4 (T.drop 1 (T.dropEnd 1 addressWithBrackets))

parseLineResults :: Parser [LineResult]
parseLineResults = manyTill parseLineResult endOfLine

parseLineResult :: Parser LineResult
parseLineResult =
  parseRequestTimedOut <|> parseGeneralFailure <|> parseReplyFrom

parseRequestTimedOut :: Parser LineResult
parseRequestTimedOut = do
  string "Request timed out."
  endOfLine
  return RequestTimedOut

parseGeneralFailure :: Parser LineResult
parseGeneralFailure = do
  string "General failure."
  endOfLine
  return GeneralFailure

parseReplyFrom :: Parser LineResult
parseReplyFrom = do
  string "Reply from "
  address <- read <$> manyTill anyChar (char ':')
  replayResult <- parseReplyResult
  return $ ReplyFrom address replayResult

parseReplyResult = parseDestinationHostUnreachable <|> parseResponseReceived

parseDestinationHostUnreachable = do
  string " Destination host unreachable."
  endOfLine
  return DestinationHostUnreachable

parseResponseReceived = do
  string " bytes="
  bytes <- decimal
  string " time"
  timeInMs <- (string "<1" *> return 0) <|> (char '=' *> decimal)
  string "ms TTL="
  ttl <- decimal
  endOfLine
  return ResponseReceived {bytes = bytes, timeInMs = timeInMs, ttl = ttl}
