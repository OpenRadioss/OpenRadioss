#!/bin/bash


pwd
echo $HOSTNAME
whoami
ip a
ifconfig -a
ip route get 1.2.3.4 | awk '{print $7}'
hostname -I
uname -a
df
lspci
