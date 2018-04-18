#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

passing_tests="tests/passing_tests/*.pxr"
failing_tests="tests/failing_tests/*.pxr"
success="SUCCESS"
failure="FAILURE"
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

for file in $passing_tests
do
  echo "${reset}running: $file"
  outfile=$(echo "$file" | cut -f 1 -d ".")".out"
  diff "$outfile" <(./pixczar.native "$file" | lli)
  if  [ $? -ne 0 ];
  then
    echo ${red}$failure ${reset}
  else
    echo ${green}$success "${reset}"
  fi
  echo "--------------------------------------"
done

for file in $failing_tests
do
  echo "${reset}running: $file"
  outfile=$(echo "$file" | cut -f 1 -d ".")".out"
  diff "$outfile" <(./pixczar.native "$file" 2>&1)
  if  [ $? -eq 0 ];
  then
    echo ${green}$failure ${reset}
  else
    echo ${red}$success or NOT FOUND"${reset}"
  fi
  echo "--------------------------------------"
done
