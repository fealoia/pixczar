#!/bin/sh


passing_tests="tests/passing_tests/*"
failing_tests="tests/failing_tests/*"

success="SUCCESS"
failure="FAILURE"

for file in $passing_tests
do
  echo "running $file \n"
  output=$(./pixczar.native $file)
  if  [ -z "$output" ];
  then
    echo $failure "\n\n"
  else
    echo $success "\n $output \n"
  fi
done

for file in $failing_tests
do
  echo "running $file \n"
  output=$(./pixczar.native $file)
  if  [ -z "$output" ];
  then
    echo $failure "\n\n"
  else
    echo $success "\n $output \n"
  fi
done
