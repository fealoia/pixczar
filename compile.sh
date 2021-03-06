#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

# needed for compilation
CC="clang"
os=`uname`
if [[ "$os" == "Darwin" ]]
then
  LIBS="-framework GLUT -framework OpenGL -framework CoreFoundation -lGLEW -lglfw -lsoil"
elif [[ "$os" == "Linux" ]]
then
  LIBS="-lGL -lglut -lGLEW -lglfw -lm -L./openGL -lSOIL"
else
  LIBS="-lGL -lglut -lGLEW -lglfw -lm -L./openGL -lSOIL"
  echo '`uname` not Darwin or Linux, compilation may fail'
fi
source ./include.sh

# file is command line argument, filename is file without extension
file=$1
filename=$(echo "$file" | cut -f 1 -d ".")
func=$2

create() { # generate code, need to pass in file since it could be modified
    ./pixczar.native "$1" > "$filename".ll
    llc "$2".ll
    eval "$CC $LIBS -o $filename.exe $filename.s opengl/main.o"
    echo "$filename.exe created"
}

# SCRIPT BEGINS HERE
if [[ $# -eq 0 ]]
then
    echo 'usage: ./compile.sh <file.pxr> [func]'
    exit 0
fi

if [ ! -e $file ]
then
    echo "file $file not found"
    exit 0
fi

if [[ "$func" == "clean" ]]
then
    echo "cleaning: $filename.s $filename.ll $filename.exe"
    rm "$filename".s "$filename".ll "$filename".exe

    include_file="$filename"_included.pxr
    if [ -e $include_file ]
    then
        echo "cleaning: $filename"_included.pxr
        rm $include_file
    fi
    exit 0
fi

echo "${reset}compiling: $file"

## following code is for handling #include
generate_includes "$file"
ret_code=$?
if [ $ret_code -eq 1 ]
then file="$(echo $file | cut -f 1 -d ".")$suffix" # use _included.pxr instead of original
fi
######################################

create "$file" "$filename"

if [[ "$func" == "run" ]]
then
echo "running: $filename.exe"
./"$filename".exe
fi
