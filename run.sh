#!/bin/sh
#kill `cat ./my.term.pid`
loc="$PWD"
cd "$loc"



gnome-terminal -e 'bash -c "service mongod start; exec bash"'
sleep 5
gnome-terminal -e 'bash -c "cd '$loc'/DirectoryServer;echo $PPID;stack exec DirectoryServer-exe; exec bash"'
sleep 5
gnome-terminal -e 'bash -c "cd '$loc'/FileServer;echo $PPID;stack exec FileServer-exe 8082; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/FileServer;echo $PPID;stack exec FileServer-exe 8081; exec bash"' 

gnome-terminal -e 'bash -c "cd '$loc'/AuthServer;echo $PPID;stack exec AuthServer-exe; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/LockingService;echo $PPID;stack exec LockingService-exe; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/TransactionServer;echo $PPID;stack exec TransactionServer-exe; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/ClientProxy;echo $PPID;stack exec ClientProxy-exe; exec bash"'