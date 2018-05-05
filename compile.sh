#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

# needed for compilation
CC="clang"
LIBS="-framework GLUT -framework OpenGL -framework CoreFoundation -lsoil -lGLEW -lglfw"
LIBS="-lGL -lglut -lGLEW -lglfw -lm -L./openGL -lSOIL"
source ./include.sh

# file is command line argument, filename is file without extension
file=$1
filename=$(echo "$file" | cut -f 1 -d ".")
func=$2

create() { # generate code, need to pass in file since it could be modified
    ./pixczar.native "$1" > "$filename".ll
    llc "$2".ll
    eval "$CC $LIBS -o $filename.exe $filename.s opengl/main.o"
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
    echo "cleaning: $filename.o $filename.s $filename.exe"
    rm "$filename".s "$filename".ll "$filename".exe

    include_file="$filename"_included.pxr
    if [ -e $include_file ]
    then
        echo "cleaning: $filename"_included.pxr
        rm $include_file
    fi
    exit 0
fi

echo "${reset}running: $file"

## following code is for handling #include
generate_includes "$file"
ret_code=$?
if [ $ret_code -eq 1 ]
then file="$(echo $file | cut -f 1 -d ".")$suffix" # use _included.pxr instead of original
fi
######################################

outfile="$filename".out
create "$file" "$filename"
