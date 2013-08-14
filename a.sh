#!/usr/bin/env bash
killall TestHack.native -s 9
./TestHack.native -f mul19.e &
./pmpa -i 1000 -p `pidof TestHack.native` 2>&1 > log