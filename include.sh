#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

# usage: include.sh <source_file.pxr>
# output: included_source_file.pxr (generates new file with included_ prefix)

include="#include  *.*"
quotes="\".*\""
# orig_file_name=$1

function generate_includes {
    ret_code=0
    input_filepath=$1
    name=$(echo $input_filepath | cut -f 1 -d ".")
    suffix="_included.pxr"
    local_path=$(dirname "${input_filepath}")

    while read line; do

        if [[ $line =~ $include ]]
        then
            arr=($line)

            to_include_filename=${arr[1]}

            # strip first and last quotes from filename, append local_path
            to_include_filename="${to_include_filename%\"}"
            to_include_filename="${to_include_filename#\"}"
            to_include_filename="$local_path/$to_include_filename"

            if [ ! -e $to_include_filename ]
            then
                echo "INCLUDE ERROR: file $to_include_filename not found" >&2
            else
                to_include="$(cat $to_include_filename)"
                # echo "$line" >&2
                # echo "$to_include" >&2

                # sed 's/"$line"/"$to_include"/' $input_filepath > "$name$suffix"
                source=$(cat $input_filepath)
                # echo "$to_include" >&2
                # echo -e hi >&2

                output="${source//"$line"/"$to_include"}"
                echo "$output" > "$name$suffix"
                ret_code=1
            fi
        fi
    done < $1

    if [ $ret_code -eq 0 ]
    then return 0
    else return 1
    fi
}

# generate_includes $1
