#!/usr/bin/env bash

for i in 1, 2, 3, 4, 5
do
  HZ=4 rake spec | fgrep Speed | cut --delim=' ' -f2
done | xargs echo | tr ' ' '+' | bc