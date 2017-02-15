#!/bin/bash

apt-get update && apt-get install -y git curl gtk-2.0

# install Stack

curl -sSL https://get.haskellstack.org/ | sh
stack setup
stack setup --upgrade-cabal

git clone  https://github.com/Garygunn94/DFS.git

# MongoDB installation

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6
echo "deb [ arch=amd64,arm64 ] http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.4 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-3.4.list
apt-get update && apt-get install -y mongodb-org

export PATH="/root/.local/bin:/root/.stack/bin:$PATH"
cd ./DFS

chmod +x clean.sh
chmod +x build.sh
chmod +x run.sh