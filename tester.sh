#!/bin/sh



passing_tests="tests/passing_tests/*"
failing_tests="tests/failing_tests/*"

success="SUCCESS"
failure="FAILURE"
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

for file in $passing_tests
do
  echo "${reset}running $file \n"
  output=$(./pixczar.native $file)
  if  [ -z "$output" ];
  then
    echo ${red}$failure ${reset}
  else
    echo ${green}$success "\n ${reset}$output"
  fi
  echo "--------------------------------------"
done

for file in $failing_tests
do
  echo "${reset}running $file \n"
  output=$(./pixczar.native $file)
  if  [ -z "$output" ];
  then
    echo ${green}$failure ${reset}
  else
    echo ${red}$success "\n ${reset}$output"
  fi
  echo "--------------------------------------"
done
