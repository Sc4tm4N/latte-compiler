#!/bin/bash

CTR=0
FLD=0
for filename in test/good/automatic/base/with_input/ret_zero/*.lat; do
  ./latc $filename > /dev/null 2> /dev/null
  compile_result=$?
  executable=$(echo $filename | rev | cut -c5- | rev)
  your_output="$executable.gen_out"
  output="$executable.out"
  input="$executable.in"
  asm="$executable.s"
  ./$executable < $input > $your_output 2> /dev/null
  run_result=$?
  diff $output $your_output > /dev/null 2> /dev/null
  diff_result=$?
  if [ $compile_result -ne 0 ]; then
    echo "$filename - failed (compiler ended with non zero status)."
    FLD=$((FLD + 1))
  elif [ $run_result -ne 0 ]; then
    echo "$filename - failed (executable ended with non zero status)."
    FLD=$((FLD + 1))
  elif [ $diff_result -ne 0 ]; then
    echo "$filename - failed (output from executable differs)."
    FLD=$((FLD + 1))
  fi
  CTR=$((CTR + 1))
  rm $executable > /dev/null 2> /dev/null
  rm $asm > /dev/null 2> /dev/null
  rm $your_output > /dev/null 2> /dev/null
  tolink="$executable.o"
  rm $tolink > /dev/null 2> /dev/null
done
echo "GOOD - automatic - base - with input - ret zero -> Failed: $FLD/$CTR"

# ======================================================================================================================

CTR=0
FLD=0
for filename in test/good/automatic/base/with_input/ret_nonzero/*.lat; do
  ./latc $filename > /dev/null 2> /dev/null
  compile_result=$?
  executable=$(echo $filename | rev | cut -c5- | rev)
  your_output="$executable.gen_out"
  output="$executable.out"
  input="$executable.in"
  asm="$executable.s"
  ./$executable < $input > $your_output 2> /dev/null
  run_result=$?
  diff $output $your_output > /dev/null 2> /dev/null
  diff_result=$?
  if [ $compile_result -ne 0 ]; then
    echo "$filename - failed (compiler ended with non zero status)."
    FLD=$((FLD + 1))
  elif [ $run_result -eq 0 ]; then
    echo "$filename - failed (executable ended with non zero status)."
    FLD=$((FLD + 1))
  elif [ $diff_result -ne 0 ]; then
    echo "$filename - failed (output from executable differs)."
    FLD=$((FLD + 1))
  fi
  CTR=$((CTR + 1))
  rm $executable > /dev/null 2> /dev/null
  rm $asm > /dev/null 2> /dev/null
  rm $your_output > /dev/null 2> /dev/null
  tolink="$executable.o"
  rm $tolink > /dev/null 2> /dev/null
done
echo "GOOD - automatic - base - with input - ret nonzero -> Failed: $FLD/$CTR"

# ======================================================================================================================

CTR=0
FLD=0
for filename in test/good/automatic/base/without_input/ret_zero/*.lat; do
  ./latc $filename > /dev/null 2> /dev/null
  compile_result=$?
  executable=$(echo $filename | rev | cut -c5- | rev)
  your_output="$executable.gen_out"
  output="$executable.out"
  asm="$executable.s"
  ./$executable > $your_output 2> /dev/null
  run_result=$?
  diff $output $your_output > /dev/null 2> /dev/null
  diff_result=$?
  if [ $compile_result -ne 0 ]; then
    echo "$filename - failed (compiler ended with non zero status)."
    FLD=$((FLD + 1))
  elif [ $run_result -ne 0 ]; then
    echo "$filename - failed (executable ended with non zero status)."
    FLD=$((FLD + 1))
  elif [ $diff_result -ne 0 ]; then
    echo "$filename - failed (output from executable differs)."
    FLD=$((FLD + 1))
  fi
  CTR=$((CTR + 1))
  rm $executable > /dev/null 2> /dev/null
  rm $asm > /dev/null 2> /dev/null
  rm $your_output > /dev/null 2> /dev/null
  tolink="$executable.o"
  rm $tolink > /dev/null 2> /dev/null
done
echo "GOOD - automatic - base - without input - ret zero -> Failed: $FLD/$CTR"


# good - without input - return code non zero
CTR=0
FLD=0
for filename in test/good/automatic/base/without_input/ret_nonzero/*.lat; do
  ./latc $filename > /dev/null 2> /dev/null
  compile_result=$?
  executable=$(echo $filename | rev | cut -c5- | rev)
  your_output="$executable.gen_out"
  output="$executable.out"
  asm="$executable.s"
  ./$executable > $your_output 2> /dev/null
  run_result=$?
  diff $output $your_output > /dev/null 2> /dev/null
  diff_result=$?
  if [ $compile_result -ne 0 ]; then
    echo "$filename - failed (compiler ended with non zero status)."
    FLD=$((FLD + 1))
  elif [ $run_result -eq 0 ]; then
    echo "$filename - failed (executable ended with zero status - expected error)."
    FLD=$((FLD + 1))
  elif [ $diff_result -ne 0 ]; then
    echo "$filename - failed (output from executable differs)."
    FLD=$((FLD + 1))
  fi
  CTR=$((CTR + 1))
  rm $executable > /dev/null 2> /dev/null
  rm $asm > /dev/null 2> /dev/null
  rm $your_output > /dev/null 2> /dev/null
  tolink="$executable.o"
  rm $tolink > /dev/null 2> /dev/null
done
echo "GOOD - automatic - base - without input - ret nonzero -> Failed: $FLD/$CTR"
