#!/bin/bash

for f in $(find lattests/bad/ -name '*.lat')
do
	echo $f
	../latc $f
	printf '\n'
done

for f in $(find mrjp-tests/bad -name '*.lat')
do
	echo $f
	../latc $f
	printf '\n'
done
