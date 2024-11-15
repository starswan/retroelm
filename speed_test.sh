#!/usr/bin/env bash

rake yarn:install
count=$1
timings=()
for i in $(seq $count)
do
  time=$(date +%T)
  value=$(HZ=2 rspec spec/features/z80_spec.rb | grep -F Speed | cut --delim=' ' -f2)
  timings+=($value)
  echo -n $time $i "Timings" ${timings[*]}
  sum=$(echo ${timings[*]} | tr ' ' '+')
  echo -n " Average "
  echo "scale=3; ($sum) / $i" | bc -l
done
