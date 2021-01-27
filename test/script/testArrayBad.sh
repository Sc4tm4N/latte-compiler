#!/bin/bash

CTR=0
FLD=0
for filename in test/bad/automatic/extension/array/compile_time/*.lat; do
  ./latc $filename > /dev/null 2> /dev/null
  compile_result=$?
  executable=$(echo $filename | rev | cut -c5- | rev)
  your_output="$executable.gen_out"
  output="$executable.out"
  asm="$executable.s"
  if [ $compile_result -eq 0 ]; then
    echo "$filename - failed (should reject)."
    FLD=$((FLD + 1))
  fi
  rm $executable > /dev/null 2> /dev/null
  rm $asm > /dev/null 2> /dev/null
  rm $your_output > /dev/null 2> /dev/null
  CTR=$((CTR + 1))
done
echo "BAD - automatic - extension - array - compile time -> Failed: $FLD/$CTR"

CTR=0
FLD=0
for filename in test/bad/automatic/extension/array/runtime/without_input/*.lat; do
  ./latc $filename > /dev/null 2> /dev/null
  compile_result=$?
  executable=$(echo $filename | rev | cut -c5- | rev)
  your_output="$executable.gen_out"
  output="$executable.out"
  asm="$executable.s"
  ./$executable > $your_output 2> /dev/null
  run_result=$?
  if [ $compile_result -ne 0 ]; then
    echo "$filename - failed (should pass)."
    FLD=$((FLD + 1))
  elif [ $run_result -eq 0 ]; then
    echo "$filename - failed (should fail)."
    FLD=$((FLD + 1))
  fi
  rm $executable > /dev/null 2> /dev/null
  rm $asm > /dev/null 2> /dev/null
  rm $your_output > /dev/null 2> /dev/null
  tolink="$executable.o"
  rm $tolink > /dev/null 2> /dev/null
  CTR=$((CTR + 1))
done
echo "BAD - automatic - array - runtime - without input -> Failed: $FLD/$CTR"
