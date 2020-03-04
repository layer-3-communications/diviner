{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}

module Diviner
  ( Log(..)
  , Ssh(..)
  , Ftp(..)
  , Request(..)
  , Session(..)
  , Peers(..)
  , Peer(..)
  , Buffer(..)
  , Channel(..)
  , decode
  ) where

import Prelude hiding (length)

import Data.Bytes.Parser (Parser,Slice(Slice),Result(Success,Failure))
import Data.Bytes.Types (Bytes(Bytes))
import Data.Word (Word16,Word64)
import Net.Types (IP)
import Data.Bytes.Parser (parseBytesMaybe)
import Foreign.C.Types (CChar)
import GHC.Exts (Ptr(Ptr))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Net.IP as IP
import qualified Data.Bytes.Types

data Log
  = LogSsh !Ssh
  | LogFtp !Ftp
  | LogChannel !Channel

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

-- | Channel logs that look like this:
--
-- > <13>Feb 12 16:21:56 JOEHOST channel: SFTP subsystem started in channel
-- > s_Id: 758614, c_Id: 0, c_Window: 131053, c_MaxPacket: 16384, s_Window:
-- > 300000, s_MaxPacket: 30000 <Host=me.example.com, SessionID=29869451,
-- > Listener=192.0.2.65:22, Client=192.0.2.117:18249, User=jdoe>
data Channel = Channel
  { server :: {-# UNPACK #-} !Buffer
  , client :: {-# UNPACK #-} !Buffer
  , session :: {-# UNPACK #-} !Session
  }

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
  { listener :: {-# UNPACK #-} !Peer
  , client :: {-# UNPACK #-} !Peer
  } deriving (Show)

data Peer = Peer
  { ip :: {-# UNPACK #-} !IP
  , port :: {-# UNPACK #-} !Word16
  } deriving (Show)

-- | Corresponds to one of these triplets of fields from the log:
--
-- * @c_id@, @c_MaxPacket@, @c_Window@
-- * @s_id@, @s_MaxPacket@, @s_Window@
data Buffer = Buffer
  { identifier :: !Word64
  , window :: !Word64
  , maxPacket :: !Word64
  }

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
       | Bytes.equalsLatin8 'c' 'h' 'a' 'n' 'n' 'e' 'l' ':' a2 ->
         if | Just structured <- Bytes.stripCStringPrefix subsystemStarted a3
            , Just channel <- parseBytesMaybe channelParser structured
              -> Just $! LogChannel channel
            | otherwise -> Nothing
       | otherwise -> Nothing
  | otherwise = Nothing

subsystemStarted :: Ptr CChar
subsystemStarted = Ptr "SFTP subsystem started in channel "#

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
      listener <- peerParser
      Latin.char9 () ',' ' ' 'C' 'l' 'i' 'e' 'n' 't' '='
      client <- peerParser
      Latin.char3 () ',' ' ' 'U'
      pure $! Just $! Peers{listener,client}
    'U' -> pure Nothing
    _ -> Parser.fail ()
  Latin.char4 () 's' 'e' 'r' '='
  user <- Parser.takeTrailedBy () 0x3E
  pure Session{host,identifier,peers,user}

peerParser :: Parser () s Peer
peerParser = do
  ip <- IP.parserUtf8Bytes ()
  Latin.char () ':'
  port <- Latin.decWord16 ()
  pure Peer{ip,port}

-- Assumes that the leading angle bracket has not yet been consumed.
requestParser :: Parser () s Request
requestParser = do
  Latin.char9 () '<' 'C' 'o' 'm' 'm' 'a' 'n' 'd' '='
  command <- Parser.takeTrailedBy () 0x2C
  -- When bytesmith-0.3.6 is released, just use char12.
  Latin.char12 () ' ' 'P' 'a' 'r' 'a' 'm' 'e' 't' 'e' 'r' 's' '='
  paramStart <- Unsafe.cursor
  !foundComma <- Parser.skipTrailedBy2 () 0x3E 0x2C
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

channelParser :: Parser () s Channel
channelParser = do
  Parser.cstring () (Ptr "s_Id: "# )
  sid <- Latin.decWord64 ()
  Parser.cstring () (Ptr ", c_Id: "# )
  cid <- Latin.decWord64 ()
  Parser.cstring () (Ptr ", c_Window: "# )
  cwindow <- Latin.decWord64 ()
  Parser.cstring () (Ptr ", c_MaxPacket: "# )
  cmaxPacket <- Latin.decWord64 ()
  let !client = Buffer
        { identifier = cid
        , window = cwindow
        , maxPacket = cmaxPacket
        }
  Parser.cstring () (Ptr ", s_Window: "# )
  swindow <- Latin.decWord64 ()
  Parser.cstring () (Ptr ", s_MaxPacket: "# )
  smaxPacket <- Latin.decWord64 ()
  let !server = Buffer
        { identifier = sid
        , window = swindow
        , maxPacket = smaxPacket
        }
  Latin.char2 () ' ' '<'
  session <- sessionParser
  pure Channel{server,client,session}
