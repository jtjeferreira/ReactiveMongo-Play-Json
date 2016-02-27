#! /bin/bash

apt-key adv --keyserver hkp://keyserver.ubuntu.com --recv EA312927

echo "deb http://repo.mongodb.org/apt/debian wheezy/mongodb-org/3.2 main" | tee /etc/apt/sources.list.d/mongodb-org-3.2.list

apt-get update
apt-get install mongodb-org-server
apt-get install mongodb-org-shell
service mongod stop

# WiredTiger
echo "Prepare MongoDB 3 configuration"

mkdir /tmp/mongo3wt
chown -R mongodb:mongodb /tmp/mongo3wt
chmod -R ug+r /tmp/mongo3wt
chmod -R u+w /tmp/mongo3wt

cat > /etc/mongod.conf <<EOF
storage:
  engine: wiredTiger
  dbPath: /tmp/mongo3wt
  journal:
    enabled: true

systemLog:
  destination: file
  logAppend: true
  path: /tmp/mongo3wt.log

net:
  port: 27017
  bindIp: 127.0.0.1
EOF

service mongod start
