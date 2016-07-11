#!/bin/bash

TESTS="/home/dillontuhy/csc453/a4/tests"
OUTS="/home/dillontuhy/csc453/a4/outs"

for i in $TESTS/test*.c; do
   n=${i%*.c}
   num=${n##*test}
   out="$OUTS/out$num"
   echo  >&2
   echo $i >&2
   ./compile < $i > test.s
   ERRCODE=$?
   spim -file test.s | tail -n+6 > tmp
   SPIMCODE=$?
   diff="test.diff"
   diff -c tmp $out > test.diff
   echo "Exit status: $ERRCODE" >&2
   echo "Spim exit status: $SPIMCODE" >&2
   if [[ (-s $diff) || ("$ERRCODE" != 0) || ("$SPIMCODE" != 0)]]; then
      cat $diff
      echo "===TEST FAILED==="

   else
      echo "===TEST PASSED==="

   fi
done
