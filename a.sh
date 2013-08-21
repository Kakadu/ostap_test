#!/usr/bin/env bash
program=TestWithAst.native
killall $program -s 9
./$program -f mul15.e -comblexbuf &
./pmpa -i 1000 -p `pidof $program` 2>&1 > log
echo "finished"

