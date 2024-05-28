#!/usr/bin/env bash

rake yarn:install
count=$1
timings=()
for i in $(seq $count)
do
  thing=`HZ=4 rake spec | fgrep Speed | cut --delim=' ' -f2`
  timings+=($thing)
done
echo "Timings" ${timings[*]}
sum=`echo ${timings[*]} | tr ' ' '+'`
echo -n "Average "
echo "scale=2; ($sum) / $count" | bc -l
