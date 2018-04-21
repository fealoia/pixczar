#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

passing_tests="tests/passing_tests/*.pxr"
failing_tests="tests/failing_tests/*.pxr"
success="SUCCESS"
failure="FAILURE"
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

CC="clang"
LIBS="-framework GLUT -framework OpenGL -lGLEW -lglfw"

./make.sh

create() {
    ./pixczar.native "$1" > "$filename".ll
    name=$(echo $1 | cut -f 1 -d ".")
    llc "$name".ll
    eval "$CC $LIBS -o $name.exe $name.s opengl/main.o"
    rm "$name".s "$name".ll
}

for file in $passing_tests
do
  echo "${reset}running: $file"
  filename=$(echo "$file" | cut -f 1 -d ".")
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
