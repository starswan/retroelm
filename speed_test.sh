#!/usr/bin/env bash

rake yarn:install
count=$1
timings=()
for i in $(seq $count)
do
  value=`HZ=4 rake spec | fgrep Speed | cut --delim=' ' -f2`
  timings+=($value)
  echo -n $i "Timings" ${timings[*]}
  sum=`echo ${timings[*]} | tr ' ' '+'`
  echo -n " Average "
  echo "scale=3; ($sum) / $i" | bc -l
done
