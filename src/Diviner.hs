{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}

module Diviner
  ( Log(..)
  , Ssh(..)
  , Ftp(..)
  , Request(..)
  , Peers(..)
  , decode
  ) where

import Prelude hiding (length)

import Data.Bytes.Parser (Parser,Slice(Slice),Result(Success,Failure))
import Data.Bytes.Types (Bytes(Bytes))
import Data.Word (Word16,Word64)
import Net.Types (IP)
import Data.Bytes.Parser (parseBytesMaybe)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Net.IP as IP
import qualified Data.Bytes.Types

data Log
  = LogSsh !Ssh
  | LogFtp !Ftp

-- | SSH server logs that look like this:
--
-- > <13>Feb 12 08:20:56 MYHOST SSH: Completed password Authentication.
-- > User logged in <Host=alpha.example.com, SessionID=02996221, 
-- > Listener=192.0.2.14:22, Client=192.0.2.67:19220, User=jdoe>
data Ssh = Ssh
  { message :: {-# UNPACK #-} !Bytes
  , session :: !Session
  } deriving (Show)

-- | SFTP and FTP server logs that look like this:
--
--  > <14>Feb 12 08:21:05 EXAMPLEHOST SFTP: Delete succeeded
--  > <Host=beta.example.com, SessionID=35067037,
--  > User=airjordan><Command=NOTIFICATION, Parameters=Notifications>
--
--  Or this (insecure FTP)
--
--  > <14>Feb 12 08:20:59 MYHOST FTP: logon success (houseprod)
--  > <Host=foo.example.com, SessionID=19833673, Listener=192.0.2.241:21,
--  > Client=192.0.2.76:49819, User=houseprod><Command=PASS,
--  > Parameters=*****, Error=220>
data Ftp = Ftp
  { ssl :: !Bool
  , message :: {-# UNPACK #-} !Bytes
  , session :: !Session
  , request :: !Request
  } deriving (Show)

data Request = Request
  { command :: {-# UNPACK #-} !Bytes
  , parameters :: {-# UNPACK #-} !Bytes
  } deriving (Show)

data Session = Session
  { host :: {-# UNPACK #-} !Bytes
  , identifier :: {-# UNPACK #-} !Word64
    -- ^ Corresponds to @SessionID@ in the log.
  , peers :: !(Maybe Peers)
  , user :: {-# UNPACK #-} !Bytes
  } deriving (Show)

data Peers = Peers
  { listenerIp :: {-# UNPACK #-} !IP
  , listenerPort :: {-# UNPACK #-} !Word16
  , clientIp :: {-# UNPACK #-} !IP
  , clientPort :: {-# UNPACK #-} !Word16
  } deriving (Show)

decode :: Bytes -> Maybe Log
decode b
  | Just postHeader <- dropSyslogHeader b
  , Just (_,a2,a3) <- Bytes.split2 0x20 postHeader =
    if | Bytes.equalsLatin4 'S' 'S' 'H' ':' a2 ->
         if | Just (rawMsg,attrs) <- Bytes.split1 0x3C a3
            , Just session <- parseBytesMaybe sessionParser attrs
              -> let message = Bytes.dropWhileEnd (==0x20) rawMsg in
                 Just $! LogSsh $! Ssh {message,session}
            | otherwise -> Nothing
       | Bytes.equalsLatin5 'S' 'F' 'T' 'P' ':' a2 ->
         if | Just (rawMsg,attrs) <- Bytes.split1 0x3C a3
            , message <- Bytes.dropWhileEnd (==0x20) rawMsg
            , Just r <- parseBytesMaybe (ftpParser True message) attrs
              -> Just $! LogFtp $! r
            | otherwise -> Nothing
       | Bytes.equalsLatin4 'F' 'T' 'P' ':' a2 ->
         if | Just (rawMsg,attrs) <- Bytes.split1 0x3C a3
            , message <- Bytes.dropWhileEnd (==0x20) rawMsg
            , Just r <- parseBytesMaybe (ftpParser False message) attrs
              -> Just $! LogFtp $! r
            | otherwise -> Nothing
       | otherwise -> Nothing
  | otherwise = Nothing

dropSyslogHeader :: Bytes -> Maybe Bytes
dropSyslogHeader b@Bytes{array} = case Parser.parseBytes headerParser b of
  Failure _ -> Nothing
  Success (Slice offset length _) -> Just (Bytes{array,offset,length})

headerParser :: Parser () s ()
headerParser = do
  Latin.char () '<'
  Latin.skipDigits1 ()
  Latin.char () '>'
  Latin.skipChar ' '
  Latin.skipTrailedBy () ' '
  Latin.skipChar ' '
  Latin.skipDigits1 ()
  Latin.skipChar1 () ' '
  Latin.skipDigits1 ()
  Latin.char () ':'
  Latin.skipDigits1 ()
  Latin.char () ':'
  Latin.skipDigits1 ()
  Latin.skipChar ' '

-- Assumes that the leading angle bracket has already been consumed.
sessionParser :: Parser () s Session
sessionParser = do
  Latin.char5 () 'H' 'o' 's' 't' '='
  host <- Parser.takeTrailedBy () 0x2C
  Latin.char11 () ' ' 'S' 'e' 's' 's' 'i' 'o' 'n' 'I' 'D' '='
  identifier <- Latin.decWord64 ()
  Latin.char2 () ',' ' '
  peers <- Latin.any () >>= \case
    'L' -> do
      Latin.char8 () 'i' 's' 't' 'e' 'n' 'e' 'r' '='
      listenerIp <- IP.parserUtf8Bytes ()
      Latin.char () ':'
      listenerPort <- Latin.decWord16 ()
      Latin.char9 () ',' ' ' 'C' 'l' 'i' 'e' 'n' 't' '='
      clientIp <- IP.parserUtf8Bytes ()
      Latin.char () ':'
      clientPort <- Latin.decWord16 ()
      Latin.char3 () ',' ' ' 'U'
      pure $! Just $! Peers{listenerIp,listenerPort,clientIp,clientPort}
    'U' -> pure Nothing
    _ -> Parser.fail ()
  Latin.char4 () 's' 'e' 'r' '='
  user <- Parser.takeTrailedBy () 0x3E
  pure Session{host,identifier,peers,user}

-- Assumes that the leading angle bracket has not yet been consumed.
requestParser :: Parser () s Request
requestParser = do
  Latin.char9 () '<' 'C' 'o' 'm' 'm' 'a' 'n' 'd' '='
  command <- Parser.takeTrailedBy () 0x2C
  -- When bytesmith-0.3.6 is released, just use char12.
  Latin.char11 () ' ' 'P' 'a' 'r' 'a' 'm' 'e' 't' 'e' 'r' 's'
  Latin.char () '='
  paramStart <- Unsafe.cursor
  !foundComma <- Parser.skipTrailedByEither () 0x2C 0x3E
  paramEndSucc <- Unsafe.cursor
  input <- Unsafe.expose
  let paramEnd = paramEndSucc - 1
      parameters = Bytes input paramStart (paramEnd - paramStart)
  case foundComma of
    False -> pure Request{command,parameters}
    True -> do
      -- Skip the Error field. Maybe this can be rethought later.
      Parser.skipTrailedBy () 0x3E
      pure Request{command,parameters}

ftpParser :: Bool -> Bytes -> Parser () s Ftp
ftpParser !ssl !message = do
  session <- sessionParser
  request <- requestParser
  pure Ftp{ssl,message,session,request}
