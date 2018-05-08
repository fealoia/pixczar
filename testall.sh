#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

passing_tests="tests/passing_tests/*.pxr"
failing_tests="tests/failing_tests/*.pxr"
success="SUCCESS"
failure="FAILURE"
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`
suffix="_included.pxr"

CC="clang"

os=`uname`
if [[ "$os" == "Darwin" ]]
then
  LIBS="-framework GLUT -framework OpenGL -framework CoreFoundation -lGLEW -lglfw"
elif [[ "$os" == "Linux" ]]
then
  LIBS="-lGL -lglut -lGLEW -lglfw -lm -L./openGL -lSOIL"
else
  LIBS="-lGL -lglut -lGLEW -lglfw -lm -L./openGL -lSOIL"
  echo '`uname` not Darwin or Linux, compilation may fail'
fi

./make.sh
source ./include.sh

create() { # generate code
    ./pixczar.native "$1" > "$filename".ll
    llc "$filename".ll
    eval "$CC $LIBS -o $filename.exe $filename.s opengl/main.o"
    rm "$filename".s "$filename".ll
}

for file in $passing_tests
do
  echo "${reset}running: $file"

  ## following code is for handling #include
  filename=$(echo "$file" | cut -f 1 -d ".") # for files .out .ll .s, etc.
  generate_includes "$file"
  ret_code=$?
  if [ $ret_code -eq 1 ]
  then file="$(echo $file | cut -f 1 -d ".")$suffix" # use _included.pxr instead of original
  fi
  ######################################

  outfile="$filename".out
  create "$file"
  diff "$outfile" <(./"$filename".exe)
  if  [ $? -ne 0 ];
  then
    echo ${red}$failure ${reset}
  else
    echo ${green}$success "${reset}"
  fi

  rm "$filename".exe
  if [ $ret_code -eq 1 ]
    then rm "$file"
  fi
  echo "--------------------------------------"
done

for file in $failing_tests
do
  echo "${reset}running: $file"

  ## following code is for handling #include
  filename=$(echo "$file" | cut -f 1 -d ".") # for files .out .ll .s, etc.
  generate_includes "$file"
  ret_code=$?
  ######################################

  outfile=$(echo "$file" | cut -f 1 -d ".")".out"
  # echo (./pixczar.native "$file" 2>&1) >&2
  diff "$outfile" <(./pixczar.native "$file" 2>&1)
  if  [ $? -eq 0 ];
  then
    echo ${green}$failure ${reset}
  else
    echo ${red}$success or NOT FOUND"${reset}"
  fi
  echo "--------------------------------------"
done
