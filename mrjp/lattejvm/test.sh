#! /bin/bash

GOOD="../test/good/*.lat"

for f in $GOOD
do
  OUTPUT=$(./latc $f)
  echo "Compiling $f: $OUTPUT"
  NAMEEXT=${f##*/}
  OUTPUT= $(cd ${f%/*}; java ${NAMEEXT%.lat} | diff -q ${NAMEEXT%.lat}.output -)
  if [ -n "$OUTPUT" ]; then
    echo "Executing $f: FAILED"
  else
    echo "Executing $f: OK"
  fi
done

