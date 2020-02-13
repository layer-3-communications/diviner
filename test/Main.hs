{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Diviner

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Bytes (Bytes)

import qualified Data.Bytes as Bytes
import qualified Data.List as List

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  when (isNothing (decode exampleA)) (fail "decode failure")
  putStrLn "B"
  when (isNothing (decode exampleB)) (fail "decode failure")
  putStrLn "C"
  when (isNothing (decode exampleC)) (fail "decode failure")
  putStrLn "D"
  when (isNothing (decode exampleD)) (fail "decode failure")
  putStrLn "E"
  when (isNothing (decode exampleE)) (fail "decode failure")
  putStrLn "Finished"

exampleA :: Bytes
exampleA = Bytes.fromLatinString $ List.concat
  [ "<13>Feb 12 08:20:56 MYHOST SSH: Completed password Authentication. "
  , "User logged in <Host=alpha.example.com, SessionID=02996221, "
  , "Listener=192.0.2.14:22, Client=192.0.2.67:19220, User=jdoe>"
  ]

exampleB :: Bytes
exampleB = Bytes.fromLatinString $ List.concat
  [ "<14>Feb 12 08:21:05 EXAMPLEHOST SFTP: Delete succeeded "
  , "<Host=beta.example.com, SessionID=35067037, "
  , "User=airjordan><Command=NOTIFICATION, Parameters=Notifications>"
  ]

exampleC :: Bytes
exampleC = Bytes.fromLatinString $ List.concat
  [ "<14>Feb 12 08:20:59 MYHOST FTP: logon success (houseprod) "
  , "<Host=foo.example.com, SessionID=19833673, Listener=192.0.2.241:21, "
  , "Client=192.0.2.76:49819, User=houseprod><Command=PASS, "
  , "Parameters=*****, Error=220>"
  ]

exampleD :: Bytes
exampleD = Bytes.fromLatinString $ List.concat
  [ "<13>Feb 12 08:21:05 THEHOST SFTP: Removed file foobar.csv "
  , "<Host=me.example.com, SessionID=45179039, Listener=192.0.2.138:22, "
  , "Client=192.0.2.214:65234, User=jdoe><Command=remove, "
  , "Parameters=foobar.csv><Filename=E:\\Program Files (x86)\\foobar.csv, "
  , "FileSize=9361519 bytes>"
  ]

exampleE :: Bytes
exampleE = Bytes.fromLatinString $ List.concat
  [ "<13>Feb 12 16:21:56 JOEHOST channel: SFTP subsystem started in channel "
  , "s_Id: 758614, c_Id: 0, c_Window: 131053, c_MaxPacket: 16384, s_Window: "
  , "300000, s_MaxPacket: 30000 <Host=me.example.com, SessionID=29869451, "
  , "Listener=192.0.2.65:22, Client=192.0.2.117:18249, User=jdoe>"
  ]
