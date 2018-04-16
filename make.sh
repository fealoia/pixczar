#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

passing_tests="tests/passing_tests/*"
failing_tests="tests/failing_tests/*"
success="SUCCESS"
failure="FAILURE"
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis pixczar.native

for file in $passing_tests
do
  echo "${reset}running $file \n"
  output=$(./"pixczar.native" $file)
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
  output=$(./"pixczar.native" $file)
  if  [ -z "$output" ];
  then
    echo ${green}$failure ${reset}
  else
    echo ${red}$success "\n ${reset}$output"
  fi
  echo "--------------------------------------"
done

./pixczar.native helloworld.pxr > "tester.ll"

lli tester.ll

output=$(lli tester.ll)

if [ "$output" == "Hello World" ];
then
	echo "helloworld test successful! 'Hello World' should be visible on terminal"
else
	echo "helloworld test failed"
fi
