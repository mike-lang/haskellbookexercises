#!/bin/bash

apt-get update

apt-get install python-software-properties -y # contains apt-add-repository command

# need libpq-dev as the postgresql client library npm module says it needs this
apt-get -y install build-essential
apt-get -y install curl
apt-get -y install make # need make for building npm modules
apt-get -y install g++ # ditto for g++

apt-get install -y git
apt-get install -y vim

#use the bash_profile to set environment variables
ln -sf /vagrant/dev/bash_profile /home/vagrant/.bash_profile

. /vagrant/dev/bash_profile
