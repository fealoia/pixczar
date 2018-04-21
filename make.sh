#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

make -C ./opengl
ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis pixczar.native
