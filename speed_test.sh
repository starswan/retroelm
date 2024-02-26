#!/usr/bin/env bash

count=$1
for i in $(seq $count)
do
  HZ=4 rake spec | fgrep Speed | cut --delim=' ' -f2
done | xargs echo | tr ' ' '+' | bc