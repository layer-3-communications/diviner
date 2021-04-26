# diviner

### Summary
This repository contains 
A grab bag for decoding logs whose formats are completely undocumented.

### Use Cases
SSH 
```sh 
--  > <13>Feb 12 08:20:56 MYHOST SSH: Completed password Authentication.  
-- > User logged in <Host=alpha.example.com, SessionID=02996221,    
-- > Listener=192.0.2.14:22, Client=192.0.2.67:19220, User=jdoe 
```

SFTP and FTP
```sh 
--  > <14>Feb 12 08:21:05 EXAMPLEHOST SFTP: Delete succeeded   
--  > <Host=beta.example.com, SessionID=35067037,   
--  > User=airjordan><Command=NOTIFICATION, Parameters=Notifications>   
```

Insecure FTP 
```sh 
--  > <14>Feb 12 08:20:59 MYHOST FTP: logon success (houseprod)   
--  > <Host=foo.example.com, SessionID=19833673, Listener=192.0.2.241:21,   
--  > Client=192.0.2.76:49819, User=houseprod><Command=PASS,   
--  > Parameters=*****, Error=220>   
```

Channel 
```sh
-- > <13>Feb 12 16:21:56 JOEHOST channel: SFTP subsystem started in channel   
-- > s_Id: 758614, c_Id: 0, c_Window: 131053, c_MaxPacket: 16384, s_Window:   
-- > 300000, s_MaxPacket: 30000 <Host=me.example.com, SessionID=29869451,   
-- > Listener=192.0.2.65:22, Client=192.0.2.117:18249, User=jdoe>   
```