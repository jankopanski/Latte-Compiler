#!/bin/bash

for f in $(find lattests/good/ -name '*.lat' | sort)
do
	echo $f
	../latc $f
	inf="./${f%.*}.input"
	if [ -f $inf ]; then
		DIFF=$( "./${f%.*}" < $inf | diff "${f%.*}.output" -)
	else
		DIFF=$( "./${f%.*}" | diff "${f%.*}.output" -)
	fi
  if [ "$DIFF" != "" ]
  then
    echo "RUN FAIL"
    echo $DIFF
  else
    echo "RUN PASS"
  fi
	printf '\n'
done

for f in $(find mrjp-tests/good/basic -name '*.lat' | sort)
do
  echo $f
  ../latc $f
	inf="./${f%.*}.input"
	if [ -f $inf ]; then
  	DIFF=$( "./${f%.*}" < $inf | diff "${f%.*}.output" -)
	else
		DIFF=$( "./${f%.*}" | diff "${f%.*}.output" -)
	fi
  if [ "$DIFF" != "" ]
  then
    echo "RUN FAIL"
    echo $DIFF
  else
    echo "RUN PASS"
  fi
	printf '\n'
done
