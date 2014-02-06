#! /bin/bash

GOOD="../testgood/*.lat"
BAD="../testbad/*.lat"
ARR="../testextensions/arrays1/*.lat"
OBJ1="../testextensions/objects1/*.lat"
OBJ2="../testextensions/objects2/*.lat"
STRUCT="../testextensions/struct/*.lat"

for f in $GOOD $ARR $OBJ1 $OBJ2 $STRUCT
do
  OUTPUT=$(./latc_llvm $f)
  echo "Compiling $f: $OUTPUT"
  OUTPUT= $(./${f%/*}/a.out | diff -q ${f%.lat}.output -)
  if [ -n "$OUTPUT" ]; then
    echo "Executing $f: FAILED"
  else
    echo "Executing $f: OK"
  fi
done

